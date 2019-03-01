{-# LANGUAGE OverloadedStrings #-}
module PostgresInit ( makePostgresDatabaseContext ) where
import Data.Foldable ( forM_ ) -- base
import qualified Data.Text as T  -- text
import qualified Data.Text.IO as T  -- text
import Database.PostgreSQL.Simple ( Connection, execute_, executeMany, query_ ) -- sqlite-simple

import AutoScheduler (  AutoSchedulerContext )
import Completions ( CompletionContext )
import DatabaseContext ( DatabaseContext(..) )
import Message ( messageToBuilderDB, messageToPattern, parseMessageUnsafe )
import PostgresAutoSchedulerContext (  makePostgresAutoSchedulerContext' )
import PostgresCompletionContext ( makePostgresCompletionContext )
import PostgresSchedulerContext ( makePostgresSchedulerContext )
import Primitive ( primitives )
import Scheduler ( SchedulerContext )
import Util ( Lock, toText )

makePostgresDatabaseContext :: Connection -> IO (DatabaseContext (Connection, Lock))
makePostgresDatabaseContext conn = do
    return $ DatabaseContext {
                initDB = do initDBPostgres conn; initPrimitivesPostgres conn,
                primitivesToHaskell = primitivesToHaskellPostgres conn,
                makeSchedulerContext = makePostgresSchedulerContext conn,
                makeAutoSchedulerContext = makePostgresAutoSchedulerContext',
                makeCompletionContext = makePostgresCompletionContext
             }

initPrimitivesPostgres :: Connection -> IO ()
initPrimitivesPostgres conn = do
    let prims = map (\(i, p, b, _) -> (i, toText (messageToBuilderDB p), b)) primitives
    () <$ executeMany conn "INSERT INTO Primitives (id, pattern, body) VALUES (?, ?, ?) \
                           \ON CONFLICT (id) DO UPDATE SET pattern = excluded.pattern, body = excluded.body" prims

primitivesToHaskellPostgres :: Connection -> IO ()
primitivesToHaskellPostgres conn = do
    prims <- query_ conn "SELECT id, pattern, body FROM Primitives" :: IO [(Int, T.Text, T.Text)]
    forM_ prims $ \(i, pattern, body) -> do
        putStr $ "prim" ++ show i ++ " "
        T.putStr (toText (messageToPattern (parseMessageUnsafe pattern)))
        putStr " = "
        T.putStrLn body

initDBPostgres :: Connection -> IO ()
initDBPostgres conn = do
    execute_ conn "\
       \CREATE TABLE IF NOT EXISTS Workspaces (\n\
       \    id SERIAL PRIMARY KEY,\n\
       \    logicalTime INTEGER NOT NULL,\n\
       \    parentWorkspaceId INTEGER NULL,\n\
       \    questionAsAsked TEXT NOT NULL,\n\
       \    questionAsAnswered TEXT NOT NULL,\n\
       \    FOREIGN KEY ( parentWorkspaceId ) REFERENCES Workspaces ( id ) ON DELETE CASCADE\n\
       \);"
    execute_ conn "CREATE INDEX IF NOT EXISTS Workspaces_IDX_ParentWorkspaces_Id ON Workspaces(parentWorkspaceId, id);"
    execute_ conn "\
       \CREATE TABLE IF NOT EXISTS Messages (\n\
       \    id SERIAL PRIMARY KEY,\n\
       \    logicalTimeSent INTEGER NOT NULL,\n\
       \    sourceWorkspaceId INTEGER NOT NULL,\n\
       \    targetWorkspaceId INTEGER NOT NULL,\n\
       \    content TEXT NOT NULL,\n\
       \    FOREIGN KEY ( sourceWorkspaceId ) REFERENCES Workspaces ( id ) ON DELETE CASCADE,\n\
       \    FOREIGN KEY ( targetWorkspaceId ) REFERENCES Workspaces ( id ) ON DELETE CASCADE\n\
       \);"
    execute_ conn "CREATE INDEX IF NOT EXISTS Messages_IDX_TargetWorkspaceId ON Messages(targetWorkspaceId);"
    execute_ conn "\
       \CREATE TABLE IF NOT EXISTS Pointers (\n\
       \    id INTEGER PRIMARY KEY,\n\
       \    content TEXT NOT NULL\n\
       \);"
    execute_ conn "\
       \CREATE TABLE IF NOT EXISTS Answers (\n\
       \    workspaceId SERIAL PRIMARY KEY, -- NOT NULL,\n\
       \    logicalTimeAnswered INTEGER NOT NULL,\n\
       \    answer TEXT NOT NULL,\n\
       \    FOREIGN KEY ( workspaceId ) REFERENCES Workspaces ( id ) ON DELETE CASCADE\n\
       \);"
    execute_ conn "\
       \CREATE TABLE IF NOT EXISTS ExpandedPointers (\n\
       \    workspaceId INTEGER NOT NULL,\n\
       \    pointerId INTEGER NOT NULL,\n\
       \    logicalTimeExpanded INTEGER NOT NULL,\n\
       \    FOREIGN KEY ( workspaceId ) REFERENCES Workspaces ( id ) ON DELETE CASCADE,\n\
       \    FOREIGN KEY ( pointerId ) REFERENCES Pointers ( id ) ON DELETE CASCADE,\n\
       \    PRIMARY KEY ( workspaceId, pointerId )\n\
       \);"
    execute_ conn "\
       \CREATE TABLE IF NOT EXISTS Commands (\n\
       \    workspaceId INTEGER NOT NULL,\n\
       \    commandTime INTEGER NOT NULL,\n\
       \    userId INTEGER NOT NULL,\n\
       \    command TEXT NOT NULL,\n\
       \    FOREIGN KEY ( workspaceId ) REFERENCES Workspaces ( id ) ON DELETE CASCADE,\n\
       \    PRIMARY KEY ( workspaceId, commandTime )\n\
       \);"
    execute_ conn "\
       \CREATE TABLE IF NOT EXISTS Functions (\n\
       \    id SERIAL PRIMARY KEY,\n\
       \    isAnswer INTEGER NOT NULL DEFAULT 0\n\
       \);"
    execute_ conn "\
       \CREATE TABLE IF NOT EXISTS Alternatives (\n\
       \    function INTEGER NOT NULL,\n\
       \    pattern TEXT NOT NULL,\n\
       \    body TEXT NOT NULL,\n\
       \    FOREIGN KEY ( function ) REFERENCES Functions ( id ) ON DELETE CASCADE,\n\
       \    PRIMARY KEY ( function, pattern )\n\
       \);"
    execute_ conn "\
       \CREATE TABLE IF NOT EXISTS Links (\n\
       \    workspaceId INTEGER NOT NULL,\n\
       \    sourceId INTEGER NOT NULL,\n\
       \    targetId INTEGER NOT NULL,\n\
       \    FOREIGN KEY ( workspaceId ) REFERENCES Workspaces ( id ) ON DELETE CASCADE,\n\
       \    FOREIGN KEY ( sourceId ) REFERENCES Pointers ( id ) ON DELETE CASCADE,\n\
       \    FOREIGN KEY ( targetId ) REFERENCES Pointers ( id ) ON DELETE CASCADE,\n\
       \    PRIMARY KEY ( workspaceId, sourceId )\n\
       \);"
    execute_ conn "\
       \CREATE TABLE IF NOT EXISTS Continuations (\n\
       \    workspaceId INTEGER NOT NULL,\n\
       \    function INTEGER NOT NULL,\n\
       \    next TEXT NOT NULL,\n\
       \    FOREIGN KEY ( function ) REFERENCES Functions ( id ) ON DELETE CASCADE,\n\
       \    PRIMARY KEY ( workspaceId, function )\n\
       \);"
    execute_ conn "\
       \CREATE TABLE IF NOT EXISTS ContinuationEnvironments (\n\
       \    workspaceId INTEGER NOT NULL,\n\
       \    function INTEGER NOT NULL,\n\
       \    variable INTEGER NOT NULL,\n\
       \    value TEXT NOT NULL,\n\
       \    FOREIGN KEY ( function ) REFERENCES Functions ( id ) ON DELETE CASCADE,\n\
       \    FOREIGN KEY ( workspaceId ) REFERENCES Workspaces ( id ) ON DELETE CASCADE,\n\
       \    FOREIGN KEY ( workspaceId, function ) REFERENCES Continuations ( workspaceId, function ) ON DELETE CASCADE,\n\
       \    PRIMARY KEY ( workspaceId, function, variable )\n\
       \);"
    execute_ conn "\
       \CREATE TABLE IF NOT EXISTS ContinuationArguments (\n\
       \    workspaceId INTEGER NOT NULL,\n\
       \    function INTEGER NOT NULL,\n\
       \    argNumber INTEGER NOT NULL,\n\
       \    value TEXT NOT NULL,\n\
       \    FOREIGN KEY ( function ) REFERENCES Functions ( id ) ON DELETE CASCADE,\n\
       \    FOREIGN KEY ( workspaceId ) REFERENCES Workspaces ( id ) ON DELETE CASCADE,\n\
       \    FOREIGN KEY ( workspaceId, function ) REFERENCES Continuations ( workspaceId, function ) ON DELETE CASCADE,\n\
       \    PRIMARY KEY ( workspaceId, function, argNumber )\n\
       \);"
    execute_ conn "\
       \CREATE TABLE IF NOT EXISTS Trace (\n\
       \    t SERIAL PRIMARY KEY,\n\
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
       \    processId SERIAL PRIMARY KEY\n\
       \);"
    execute_ conn "\
       \CREATE TABLE IF NOT EXISTS Sessions (\n\
       \    sessionId SERIAL PRIMARY KEY\n\
       \);"
    execute_ conn "\
       \CREATE TABLE IF NOT EXISTS SessionProcesses (\n\
       \    sessionId INTEGER NOT NULL,\n\
       \    processId INTEGER NOT NULL,\n\
       \    FOREIGN KEY ( sessionId ) REFERENCES Sessions ( sessionId ) ON DELETE CASCADE,\n\
       \    PRIMARY KEY ( sessionId, processId )\n\
       \);"
    execute_ conn "\
       \CREATE TABLE IF NOT EXISTS Primitives (\n\
       \    id INTEGER PRIMARY KEY,\n\
       \    pattern TEXT NOT NULL,\n\
       \    body TEXT NOT NULL\n\
       \);"
    return ()
