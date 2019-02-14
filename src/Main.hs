{-# LANGUAGE OverloadedStrings #-}
module Main where
import Control.Applicative ( optional, (<|>) ) -- base
import Data.Foldable ( forM_ ) -- base
import qualified Data.Map as M -- containers
import qualified Data.Text as T -- text
import qualified Data.Text.IO as T -- text
import Database.SQLite.Simple ( Connection, withConnection, execute_, executeMany, query_ ) -- sqlite-simple
import Network.Wai.Handler.Warp ( run ) -- warp
import Options.Applicative ( Parser, command, hsubparser, auto, execParser, info, progDesc, switch, str, helper,
                             long, option, footer, argument, help, metavar, value, internal ) -- optparse-applicative
import Servant ( Proxy(..) ) -- servant-server
import Servant.JS ( writeJSForAPI, axios, defAxiosOptions ) -- servant-js
import System.Environment ( getArgs ) -- base

import AutoInterpreter ( runM, makeInterpreterScheduler, )
import AutoScheduler ( schedulerContext, allAlternatives )
import CommandLine ( commandLineInteraction )
import Exp ( Exp(..), Name(..), expToHaskell )
import Message ( messageToHaskell, messageToBuilderDB, messageToPattern, parseMessageUnsafe )
import Primitive ( primitives )
import Scheduler ( SessionId, newSession, getWorkspace, createInitialWorkspace, makeSingleUserScheduler, fullyExpand )
import SqliteAutoSchedulerContext ( makeSqliteAutoSchedulerContext' )
import SqliteSchedulerContext ( makeSqliteSchedulerContext )
import Server ( API, initServer )
import Util ( toText )
import Workspace ( identity )

data Options
    = GenAPI
    | Serve Bool FilePath
    | CommandLine Bool Bool (Maybe SessionId) FilePath
    | Export FilePath SessionId
  deriving ( Show )

noAutoOption :: Parser Bool
noAutoOption = switch (long "no-auto" <> help "Disable automation")

concurrentOption :: Parser Bool
concurrentOption = switch (long "concurrent" <> help "Allow concurrent execution.")

dbFileOption :: Parser FilePath
dbFileOption = argument str (metavar "DB" <> value ":memory:" <> help "Database file")

sessionOption :: Parser SessionId
sessionOption = argument auto (metavar "SESSIONID" <> help "Session ID")

sessionIdOption :: Parser SessionId
sessionIdOption = option auto (long "session-id" <> metavar "SESSIONID" <> help "Session ID")

optionsParser :: Parser Options
optionsParser = hsubparser $ mconcat [
    command "gen-api" (info (pure GenAPI)
                      ({-internal <> -}progDesc "Generate ts/command-api.js")),
    command "serve" (info (Serve <$> noAutoOption <*> dbFileOption)
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
            withConnection dbFile $ \conn -> do
                ctxt <- makeSqliteSchedulerContext conn
                autoCtxt <- makeSqliteAutoSchedulerContext' sessionId ctxt
                alts <- fmap reverse <$> allAlternatives autoCtxt
                let localLookup f = maybe [] id $ M.lookup f alts
                case M.lookup ANSWER alts of
                    Nothing -> putStrLn $ "Session " ++ show sessionId ++ " not found."
                    Just root@(([topArg], _):_) -> do
                        putStrLn "{-# LANGUAGE OverloadedStrings #-}"
                        putStrLn "import Data.String ( IsString(..) )"
                        primitivesToHaskell conn
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
        Serve noAuto dbFile -> do
            withConnection dbFile $ \conn -> do
                initSqlite conn
                initPrimitives conn
                putStrLn "Navigate to http://localhost:8081/static/index.html ..."
                run 8081 =<< ({-if noAuto then initServerNoAutomation else-} initServer) conn
        CommandLine True _ mSessionId dbFile -> do -- TODO: Use session ID.
            withConnection dbFile $ \conn -> do
                initSqlite conn
                ctxt <- makeSqliteSchedulerContext conn
                initWorkspace <- getWorkspace ctxt =<< createInitialWorkspace ctxt
                scheduler <- makeSingleUserScheduler ctxt
                commandLineInteraction initWorkspace scheduler
        CommandLine False concurrent mSessionId dbFile -> do
            withConnection dbFile $ \conn -> do
                initSqlite conn
                initPrimitives conn
                ctxt <- makeSqliteSchedulerContext conn
                sessionId <- maybe (newSession ctxt) return mSessionId
                putStrLn ("Session ID: " ++ show sessionId)
                autoCtxt <- makeSqliteAutoSchedulerContext' sessionId ctxt
                let !ctxt = schedulerContext autoCtxt
                initWorkspace <- getWorkspace ctxt =<< createInitialWorkspace ctxt
                scheduler <- runM (makeInterpreterScheduler (not concurrent) autoCtxt $! identity initWorkspace) 0
                commandLineInteraction initWorkspace scheduler

-- TODO: Move this elsewhere.
initPrimitives :: Connection -> IO ()
initPrimitives conn = do
    let prims = map (\(i, p, b, _) -> (i, toText (messageToBuilderDB p), b)) primitives
    executeMany conn "INSERT OR REPLACE INTO Primitives (id, pattern, body) VALUES (?, ?, ?)" prims

primitivesToHaskell :: Connection -> IO ()
primitivesToHaskell conn = do
    prims <- query_ conn "SELECT id, pattern, body FROM Primitives" :: IO [(Int, T.Text, T.Text)]
    forM_ prims $ \(i, pattern, body) -> do
        putStr $ "prim" ++ show i ++ " "
        T.putStr (toText (messageToPattern (parseMessageUnsafe pattern)))
        putStr " = "
        T.putStrLn body

-- TODO: Move this elsewhere at some point.
initSqlite :: Connection -> IO ()
initSqlite conn = do
    execute_ conn "PRAGMA journal_mode = WAL;" -- Improves speed significantly when writing to a file.
    execute_ conn "PRAGMA synchronous = OFF;" -- Evil, but makes it even faster and should be fine enough for testing.
    execute_ conn "\
       \CREATE TABLE IF NOT EXISTS Workspaces (\n\
       \    id INTEGER PRIMARY KEY ASC,\n\
       \    logicalTime INTEGER NOT NULL,\n\
       \    parentWorkspaceId INTEGER NULL,\n\
       \    questionAsAsked TEXT NOT NULL,\n\
       \    questionAsAnswered TEXT NOT NULL,\n\
       \    FOREIGN KEY ( parentWorkspaceId ) REFERENCES Workspaces ( id ) ON DELETE CASCADE\n\
       \);"
    execute_ conn "CREATE INDEX IF NOT EXISTS Workspaces_IDX_ParentWorkspaces_Id ON Workspaces(parentWorkspaceId, id);"
    execute_ conn "\
       \CREATE TABLE IF NOT EXISTS Messages (\n\
       \    id INTEGER PRIMARY KEY ASC,\n\
       \    logicalTimeSent INTEGER NOT NULL,\n\
       \    sourceWorkspaceId INTEGER NOT NULL,\n\
       \    targetWorkspaceId INTEGER NOT NULL,\n\
       \    content TEXT NOT NULL,\n\
       \    FOREIGN KEY ( sourceWorkspaceId ) REFERENCES Workspaces ( id ) ON DELETE CASCADE\n\
       \    FOREIGN KEY ( targetWorkspaceId ) REFERENCES Workspaces ( id ) ON DELETE CASCADE\n\
       \);"
    execute_ conn "CREATE INDEX IF NOT EXISTS Messages_IDX_TargetWorkspaceId ON Messages(targetWorkspaceId);"
    execute_ conn "\
       \CREATE TABLE IF NOT EXISTS Pointers (\n\
       \    id INTEGER PRIMARY KEY ASC,\n\
       \    content TEXT NOT NULL\n\
       \);"
    execute_ conn "\
       \CREATE TABLE IF NOT EXISTS Answers (\n\
       \    workspaceId INTEGER PRIMARY KEY ASC, -- NOT NULL,\n\
       \    logicalTimeAnswered INTEGER NOT NULL,\n\
       \    answer TEXT NOT NULL,\n\
       \    FOREIGN KEY ( workspaceId ) REFERENCES Workspaces ( id ) ON DELETE CASCADE\n\
       \);"
    execute_ conn "\
       \CREATE TABLE IF NOT EXISTS ExpandedPointers (\n\
       \    workspaceId INTEGER NOT NULL,\n\
       \    pointerId INTEGER NOT NULL,\n\
       \    logicalTimeExpanded INTEGER NOT NULL,\n\
       \    FOREIGN KEY ( workspaceId ) REFERENCES Workspaces ( id ) ON DELETE CASCADE\n\
       \    FOREIGN KEY ( pointerId ) REFERENCES Pointers ( id ) ON DELETE CASCADE\n\
       \    PRIMARY KEY ( workspaceId ASC, pointerId ASC )\n\
       \);"
    execute_ conn "\
       \CREATE TABLE IF NOT EXISTS Commands (\n\
       \    workspaceId INTEGER NOT NULL,\n\
       \    localTime INTEGER NOT NULL,\n\
       \    userId INTEGER NOT NULL,\n\
       \    command TEXT NOT NULL,\n\
       \    FOREIGN KEY ( workspaceId ) REFERENCES Workspaces ( id ) ON DELETE CASCADE\n\
       \    PRIMARY KEY ( workspaceId ASC, localTime ASC )\n\
       \);"
    execute_ conn "\
       \CREATE TABLE IF NOT EXISTS Functions (\n\
       \    id INTEGER PRIMARY KEY ASC,\n\
       \    isAnswer INTEGER NOT NULL DEFAULT 0\n\
       \);"
    execute_ conn "\
       \CREATE TABLE IF NOT EXISTS Alternatives (\n\
       \    function INTEGER NOT NULL,\n\
       \    pattern TEXT NOT NULL,\n\
       \    body TEXT NOT NULL,\n\
       \    FOREIGN KEY ( function ) REFERENCES Functions ( id ) ON DELETE CASCADE\n\
       \    PRIMARY KEY ( function ASC, pattern ASC )\n\
       \);"
    execute_ conn "\
       \CREATE TABLE IF NOT EXISTS Continuations (\n\
       \    workspaceId INTEGER NOT NULL,\n\
       \    function INTEGER NOT NULL,\n\
       \    next TEXT NOT NULL,\n\
       \    FOREIGN KEY ( function ) REFERENCES Functions ( id ) ON DELETE CASCADE\n\
       \    PRIMARY KEY ( workspaceId ASC, function ASC )\n\
       \);"
    execute_ conn "\
       \CREATE TABLE IF NOT EXISTS ContinuationEnvironments (\n\
       \    workspaceId INTEGER NOT NULL,\n\
       \    function INTEGER NOT NULL,\n\
       \    variable INTEGER NOT NULL,\n\
       \    value TEXT NOT NULL,\n\
       \    FOREIGN KEY ( function ) REFERENCES Functions ( id ) ON DELETE CASCADE\n\
       \    FOREIGN KEY ( workspaceId ) REFERENCES Workspaces ( id ) ON DELETE CASCADE\n\
       \    FOREIGN KEY ( workspaceId, function ) REFERENCES Continuations ( workspaceId, function ) ON DELETE CASCADE\n\
       \    PRIMARY KEY ( workspaceId ASC, function ASC, variable ASC )\n\
       \);"
    execute_ conn "\
       \CREATE TABLE IF NOT EXISTS ContinuationArguments (\n\
       \    workspaceId INTEGER NOT NULL,\n\
       \    function INTEGER NOT NULL,\n\
       \    argNumber INTEGER NOT NULL,\n\
       \    value TEXT NOT NULL,\n\
       \    FOREIGN KEY ( function ) REFERENCES Functions ( id ) ON DELETE CASCADE\n\
       \    FOREIGN KEY ( workspaceId ) REFERENCES Workspaces ( id ) ON DELETE CASCADE\n\
       \    FOREIGN KEY ( workspaceId, function ) REFERENCES Continuations ( workspaceId, function ) ON DELETE CASCADE\n\
       \    PRIMARY KEY ( workspaceId ASC, function ASC, argNumber ASC )\n\
       \);"
    execute_ conn "\
       \CREATE TABLE IF NOT EXISTS Trace (\n\
       \    t INTEGER PRIMARY KEY ASC,\n\
       \    processId INTEGER NOT NULL,\n\
       \    varEnv TEXT NOT NULL,\n\
       \    funEnv TEXT NOT NULL,\n\
       \    workspaceId INTEGER NOT NULL,\n\
       \    expression TEXT NOT NULL,\n\
       \    continuation TEXT NOT NULL,\n\
       \    FOREIGN KEY ( workspaceId ) REFERENCES Workspaces ( id ) ON DELETE CASCADE\n\
       \);"
    execute_ conn "\
       \CREATE TABLE IF NOT EXISTS RunQueue (\n\
       \    processId INTEGER PRIMARY KEY ASC\n\
       \);"
    execute_ conn "\
       \CREATE TABLE IF NOT EXISTS Sessions (\n\
       \    sessionId INTEGER PRIMARY KEY ASC\n\
       \);"
    execute_ conn "\
       \CREATE TABLE IF NOT EXISTS SessionProcesses (\n\
       \    sessionId INTEGER NOT NULL,\n\
       \    processId INTEGER UNIQUE NOT NULL,\n\
       \    FOREIGN KEY ( sessionId ) REFERENCES Sessions ( sessionId ) ON DELETE CASCADE\n\
       \    PRIMARY KEY ( sessionId ASC, processId ASC )\n\
       \);"
    execute_ conn "\
       \CREATE TABLE IF NOT EXISTS Primitives (\n\
       \    id INTEGER PRIMARY KEY ASC,\n\
       \    pattern TEXT NOT NULL,\n\
       \    body TEXT NOT NULL\n\
       \);"
