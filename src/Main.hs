{-# LANGUAGE OverloadedStrings #-}
module Main where
import Control.Applicative ( optional, (<|>) ) -- base
import Data.Foldable ( forM_ ) -- base
import Data.List ( isPrefixOf ) -- base
import qualified Data.Map as M -- containers
import qualified Data.Text as T -- text
import qualified Data.Text.IO as T -- text
import qualified Database.PostgreSQL.Simple as Postgres ( connect ) -- postgresql-simple
import Database.PostgreSQL.Simple.URL ( parseDatabaseUrl ) -- postgresql-simple-url
import qualified Database.SQLite.Simple as Sqlite ( withConnection ) -- sqlite-simple
import Network.Wai.Handler.Warp ( Port, run, defaultSettings, setPort ) -- warp
import Network.Wai.Handler.WarpTLS ( runTLS, tlsSettings ) -- warp-tls
import Options.Applicative ( Parser, command, hsubparser, auto, execParser, info, progDesc, switch, str, helper, strOption,
                             long, option, footer, argument, help, metavar, value, internal ) -- optparse-applicative
import Servant ( Proxy(..) ) -- servant-server
import Servant.JS ( writeJSForAPI, axios, defAxiosOptions ) -- servant-js
import System.Environment ( getArgs ) -- base

import AutoInterpreter ( runM, makeInterpreterScheduler, )
import AutoScheduler ( schedulerContext, allAlternatives )
import CommandLine ( commandLineInteraction )
import DatabaseContext ( DatabaseContext(..) )
import Exp ( Exp(..), Name(..), expToHaskell )
import Message ( messageToHaskell, messageToBuilderDB, messageToPattern, parseMessageUnsafe )
import Scheduler ( SessionId, newSession, getWorkspace, createInitialWorkspace, makeSingleUserScheduler, fullyExpand )
import PostgresInit ( makePostgresDatabaseContext )
import SqliteInit ( makeSqliteDatabaseContext )
import Server ( API, initServer )
import Util ( toText )
import Workspace ( identity )

data Options
    = GenAPI
    | Serve Bool FilePath Port (Maybe FilePath) (Maybe FilePath)
    | CommandLine Bool Bool (Maybe SessionId) FilePath
    | Export FilePath SessionId
  deriving ( Show )

noAutoOption :: Parser Bool
noAutoOption = switch (long "no-auto" <> help "Disable automation")

concurrentOption :: Parser Bool
concurrentOption = switch (long "concurrent" <> help "Allow concurrent execution.")

dbFileOption :: Parser FilePath
dbFileOption = argument str (metavar "DB" <> value ":memory:" <> help "Database file")

portOption :: Parser Port
portOption = option auto (long "port" <> metavar "PORT" <> value 8081 <> help "Port for web server to listen on (default: 8081)")

certFileOption :: Parser FilePath
certFileOption = strOption (long "certificate" <> metavar "FILE" <> help "Path to server certificate")

keyFileOption :: Parser FilePath
keyFileOption = strOption (long "key-file" <> metavar "FILE" <> help "Path to certificate's private key")

sessionOption :: Parser SessionId
sessionOption = argument auto (metavar "SESSIONID" <> help "Session ID")

sessionIdOption :: Parser SessionId
sessionIdOption = option auto (long "session-id" <> metavar "SESSIONID" <> help "Session ID")

optionsParser :: Parser Options
optionsParser = hsubparser $ mconcat [
    command "gen-api" (info (pure GenAPI)
                      ({-internal <> -}progDesc "Generate ts/command-api.js")),
    command "serve" (info (Serve <$> noAutoOption <*> dbFileOption <*> portOption <*> optional certFileOption <*> optional keyFileOption)
                    (progDesc "Start webserver.")),
    command "export" (info (Export <$> dbFileOption <*> sessionOption)
                     (progDesc "Print automation code as Haskell.")),
    command "start" (info (CommandLine <$> noAutoOption <*> concurrentOption <*> optional sessionIdOption <*> dbFileOption)
                    (progDesc "Start command-line interaction."))]

main :: IO ()
main = do
    options <- execParser (info (helper <*> optionsParser) (footer "Run COMMAND --help for help on each command."))
    case options of
        GenAPI -> writeJSForAPI (Proxy :: Proxy API) (axios defAxiosOptions) "ts/command-api.js"
        Export dbFile sessionId -> do
            -- TODO: XXX if "postgres://" `isPrefixOf` dbFile then parseDatabaseUrl dbFile else sqlite
            Sqlite.withConnection dbFile $ \conn -> do
                dbCtxt <- makeSqliteDatabaseContext conn
                ctxt <- makeSchedulerContext dbCtxt
                autoCtxt <- makeAutoSchedulerContext dbCtxt ctxt sessionId
                alts <- fmap reverse <$> allAlternatives autoCtxt
                let localLookup f = maybe [] id $ M.lookup f alts
                case M.lookup ANSWER alts of
                    Nothing -> putStrLn $ "Session " ++ show sessionId ++ " not found."
                    Just root@(([topArg], _):_) -> do
                        putStrLn "{-# LANGUAGE OverloadedStrings #-}"
                        putStrLn "import Data.String ( IsString(..) )"
                        primitivesToHaskell dbCtxt
                        putStrLn "\ndata Message = T String | S [Message]"
                        putStrLn "instance IsString Message where fromString = T"
                        putStrLn "instance Show Message where\n\
                                 \    showsPrec _ (T s) = (s++)\n\
                                 \    showsPrec 0 (S ms) = foldr (.) id (map (showsPrec 1) ms)\n\
                                 \    showsPrec _ (S ms) = ('[':) . foldr (.) id (map (showsPrec 1) ms) . (']':)"
                        -- or, putStrLn "data Message = T String | S [Message] deriving (Show)"
                        putStr "\nmain = print $ "
                        topArg <- fullyExpand (schedulerContext autoCtxt) topArg
                        T.putStrLn (toText (expToHaskell localLookup (LetFun ANSWER (Call ANSWER [Value topArg]))))
        Serve noAuto dbFile port mCertFile mKeyFile -> do
            -- TODO: XXX if "postgres://" `isPrefixOf` dbFile then parseDatabaseUrl dbFile else sqlite
            Sqlite.withConnection dbFile $ \conn -> do
                dbCtxt <- makeSqliteDatabaseContext conn
                initDB dbCtxt
                case (mCertFile, mKeyFile) of
                    (Nothing, Nothing) -> do
                        putStrLn $ "Navigate to http://localhost:"++show port++"/static/index.html ..."
                        run port =<< ({-if noAuto then initServerNoAutomation else-} initServer) dbCtxt
                    (Just certFile, Just keyFile) -> do
                        let !tlsOpts = tlsSettings certFile keyFile
                            !warpOpts = setPort port defaultSettings
                        putStrLn $ "Navigate to https://localhost:"++show port++"/static/index.html ..."
                        runTLS tlsOpts warpOpts =<< ({-if noAuto then initServerNoAutomation else-} initServer) dbCtxt
                    _ -> putStrLn "Both a certificate and a key are needed to enable TLS."
        CommandLine True _ mSessionId dbFile -> do -- TODO: Use session ID.
            -- TODO: XXX if "postgres://" `isPrefixOf` dbFile then parseDatabaseUrl dbFile else sqlite
            Sqlite.withConnection dbFile $ \conn -> do
                dbCtxt <- makeSqliteDatabaseContext conn
                initDB dbCtxt
                ctxt <- makeSchedulerContext dbCtxt
                initWorkspace <- getWorkspace ctxt =<< createInitialWorkspace ctxt
                scheduler <- makeSingleUserScheduler ctxt
                commandLineInteraction initWorkspace scheduler
        CommandLine False concurrent mSessionId dbFile -> do
            -- TODO: XXX if "postgres://" `isPrefixOf` dbFile then parseDatabaseUrl dbFile else sqlite
            Sqlite.withConnection dbFile $ \conn -> do
                dbCtxt <- makeSqliteDatabaseContext conn
                initDB dbCtxt
                ctxt <- makeSchedulerContext dbCtxt
                sessionId <- newSession ctxt mSessionId
                putStrLn ("Session ID: " ++ show sessionId)
                autoCtxt <- makeAutoSchedulerContext dbCtxt ctxt sessionId
                let !ctxt = schedulerContext autoCtxt
                initWorkspace <- getWorkspace ctxt =<< createInitialWorkspace ctxt
                scheduler <- runM (makeInterpreterScheduler (not concurrent) autoCtxt $! identity initWorkspace) 0
                commandLineInteraction initWorkspace scheduler
