{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
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
import Message ( Message(..), messageToHaskell, messageToBuilderDB, messageToPattern, parseMessageUnsafe )
import Scheduler ( SessionId, newSession, getWorkspace, createInitialWorkspace, labelMessage, makeSingleUserScheduler, fullyExpand )
import Caching.Init ( makeCachingDatabaseContext )
import Postgres.Init ( makePostgresDatabaseContext )
import Sqlite.Init ( makeSqliteDatabaseContext )
import Server ( API, initServer )
import Util ( toText )
import Workspace ( identity )

data Options
    = GenAPI
    | Serve Bool Bool FilePath Port (Maybe FilePath) (Maybe FilePath)
    | CommandLine Bool Bool Bool (Maybe SessionId) FilePath
    | Export FilePath SessionId
  deriving ( Show )

noAutoOption :: Parser Bool
noAutoOption = switch (long "no-auto" <> help "Disable automation")

concurrentOption :: Parser Bool
concurrentOption = switch (long "concurrent" <> help "Allow concurrent execution")

dbFileOption :: Parser FilePath
dbFileOption = argument str (metavar "DB" <> value ":memory:" <> help "Database file")

noCacheOption :: Parser Bool
noCacheOption = switch (long "no-cache" <> help "Disable caching")

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
    command "serve" (info (Serve <$> noAutoOption <*> noCacheOption <*> dbFileOption <*> portOption <*> optional certFileOption <*> optional keyFileOption)
                    (progDesc "Start webserver.")),
    command "export" (info (Export <$> dbFileOption <*> sessionOption)
                     (progDesc "Print automation code as Haskell.")),
    command "start" (info (CommandLine <$> noAutoOption <*> noCacheOption <*> concurrentOption <*> optional sessionIdOption <*> dbFileOption)
                    (progDesc "Start command-line interaction."))]

withDatabaseContext :: Bool -> FilePath -> (forall e. DatabaseContext e -> IO ()) -> IO ()
withDatabaseContext noCache dbFile body = do
    -- Example: "postgres://foo:bar@example.com:2345/database" becomes:
    -- ConnectInfo {connectHost = "example.com", connectPort = 2345, connectUser = "foo", connectPassword = "bar", connectDatabase = "database"}
    if ("postgres://" `isPrefixOf` dbFile) || ("postgresql://" `isPrefixOf` dbFile) then do
        case parseDatabaseUrl dbFile of
            Nothing -> putStrLn $ "'" ++ dbFile ++ "' isn't a valid PostgreSQL URL."
            Just connInfo -> do
                conn <- Postgres.connect connInfo
                dbCtxt <- makePostgresDatabaseContext conn
                dbCtxt <- if noCache then return dbCtxt else makeCachingDatabaseContext dbCtxt
                initDB dbCtxt
                body dbCtxt
                closeDB dbCtxt
      else do
        Sqlite.withConnection dbFile $ \conn -> do
            dbCtxt <- makeSqliteDatabaseContext conn
            dbCtxt <- if noCache then return dbCtxt else makeCachingDatabaseContext dbCtxt
            initDB dbCtxt
            body dbCtxt
            closeDB dbCtxt

topLevelQuestion :: Message
topLevelQuestion = Text "What is your question?"

main :: IO ()
main = do
    options <- execParser (info (helper <*> optionsParser) (footer "Run COMMAND --help for help on each command."))
    case options of
        GenAPI -> writeJSForAPI (Proxy :: Proxy API) (axios defAxiosOptions) "ts/command-api.js"
        Export dbFile sessionId -> do
            withDatabaseContext True dbFile $ \dbCtxt -> do
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
        Serve noAuto noCache dbFile port mCertFile mKeyFile -> do
            withDatabaseContext noCache dbFile $ \dbCtxt -> do
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
        CommandLine True noCache _ mSessionId dbFile -> do -- TODO: Use session ID.
            withDatabaseContext True dbFile $ \dbCtxt -> do -- TODO: Haven't tested no automation with cache.
                ctxt <- makeSchedulerContext dbCtxt
                msg <- labelMessage ctxt topLevelQuestion
                initWorkspace <- getWorkspace ctxt =<< createInitialWorkspace ctxt msg
                scheduler <- makeSingleUserScheduler ctxt
                commandLineInteraction initWorkspace scheduler
        CommandLine False noCache concurrent mSessionId dbFile -> do
            withDatabaseContext noCache dbFile $ \dbCtxt -> do
                ctxt <- makeSchedulerContext dbCtxt
                sessionId <- newSession ctxt mSessionId
                putStrLn ("Session ID: " ++ show sessionId)
                autoCtxt <- makeAutoSchedulerContext dbCtxt ctxt sessionId
                let !ctxt = schedulerContext autoCtxt
                msg <- labelMessage ctxt topLevelQuestion
                initWorkspace <- getWorkspace ctxt =<< createInitialWorkspace ctxt msg
                scheduler <- runM (makeInterpreterScheduler (not concurrent) autoCtxt $! identity initWorkspace)
                commandLineInteraction initWorkspace scheduler
