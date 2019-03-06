{-# LANGUAGE OverloadedStrings #-}
module Sqlite.Init ( makeSqliteDatabaseContext ) where
import Data.Foldable ( forM_ ) -- base
import qualified Data.Text as T  -- text
import qualified Data.Text.IO as T  -- text
import Database.SQLite.Simple ( Connection, execute_, executeMany, query_ ) -- sqlite-simple

import AutoScheduler (  AutoSchedulerContext )
import Completions ( CompletionContext )
import DatabaseContext ( DatabaseContext(..) )
import Message ( messageToBuilderDB, messageToPattern, parseMessageUnsafe )
import Primitive ( primitives )
import Scheduler ( SchedulerContext )
import Sqlite.AutoSchedulerContext (  makeSqliteAutoSchedulerContext' )
import Sqlite.CompletionContext ( makeSqliteCompletionContext )
import Sqlite.SchedulerContext ( makeSqliteSchedulerContext )
import Util ( Queue, toText )

makeSqliteDatabaseContext :: Connection -> IO (DatabaseContext (Connection, Queue))
makeSqliteDatabaseContext conn = do
    return $ DatabaseContext {
                initDB = do initDBSqlite conn; initPrimitivesSqlite conn,
                primitivesToHaskell = primitivesToHaskellSqlite conn,
                makeSchedulerContext = makeSqliteSchedulerContext conn,
                makeAutoSchedulerContext = makeSqliteAutoSchedulerContext',
                makeCompletionContext = makeSqliteCompletionContext
             }

initPrimitivesSqlite :: Connection -> IO ()
initPrimitivesSqlite conn = do
    let prims = map (\(i, p, b, _) -> (i, toText (messageToBuilderDB p), b)) primitives
    executeMany conn "INSERT OR REPLACE INTO Primitives (id, pattern, body) VALUES (?, ?, ?)" prims

primitivesToHaskellSqlite :: Connection -> IO ()
primitivesToHaskellSqlite conn = do
    prims <- query_ conn "SELECT id, pattern, body FROM Primitives" :: IO [(Int, T.Text, T.Text)]
    forM_ prims $ \(i, pattern, body) -> do
        putStr $ "prim" ++ show i ++ " "
        T.putStr (toText (messageToPattern (parseMessageUnsafe pattern)))
        putStr " = "
        T.putStrLn body

initDBSqlite :: Connection -> IO ()
initDBSqlite conn = do
    execute_ conn "PRAGMA journal_mode = WAL;" -- Improves speed significantly when writing to a file.
    execute_ conn "PRAGMA synchronous = OFF;" -- Evil, but makes it even faster and should be fine enough for testing.
    execute_ conn "\
       \CREATE TABLE IF NOT EXISTS Workspaces (\n\
       \    id TEXT PRIMARY KEY ASC,\n\
       \    logicalTime INTEGER NOT NULL,\n\
       \    parentWorkspaceId TEXT NULL,\n\
       \    questionAsAsked TEXT NOT NULL,\n\
       \    questionAsAnswered TEXT NOT NULL,\n\
       \    FOREIGN KEY ( parentWorkspaceId ) REFERENCES Workspaces ( id ) ON DELETE CASCADE\n\
       \);"
    execute_ conn "CREATE INDEX IF NOT EXISTS Workspaces_IDX_ParentWorkspaces_Id ON Workspaces(parentWorkspaceId, id);"
    execute_ conn "\
       \CREATE TABLE IF NOT EXISTS Messages (\n\
       \    id INTEGER PRIMARY KEY ASC,\n\
       \    logicalTimeSent INTEGER NOT NULL,\n\
       \    sourceWorkspaceId TEXT NOT NULL,\n\
       \    targetWorkspaceId TEXT NOT NULL,\n\
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
       \    workspaceId TEXT PRIMARY KEY ASC, -- NOT NULL,\n\
       \    logicalTimeAnswered INTEGER NOT NULL,\n\
       \    answer TEXT NOT NULL,\n\
       \    FOREIGN KEY ( workspaceId ) REFERENCES Workspaces ( id ) ON DELETE CASCADE\n\
       \);"
    execute_ conn "\
       \CREATE TABLE IF NOT EXISTS ExpandedPointers (\n\
       \    workspaceId TEXT NOT NULL,\n\
       \    pointerId INTEGER NOT NULL,\n\
       \    logicalTimeExpanded INTEGER NOT NULL,\n\
       \    FOREIGN KEY ( workspaceId ) REFERENCES Workspaces ( id ) ON DELETE CASCADE\n\
       \    FOREIGN KEY ( pointerId ) REFERENCES Pointers ( id ) ON DELETE CASCADE\n\
       \    PRIMARY KEY ( workspaceId ASC, pointerId ASC )\n\
       \);"
    execute_ conn "\
       \CREATE TABLE IF NOT EXISTS Commands (\n\
       \    workspaceId TEXT NOT NULL,\n\
       \    commandTime INTEGER NOT NULL,\n\
       \    userId TEXT NOT NULL,\n\
       \    command TEXT NOT NULL,\n\
       \    FOREIGN KEY ( workspaceId ) REFERENCES Workspaces ( id ) ON DELETE CASCADE\n\
       \    PRIMARY KEY ( workspaceId ASC, commandTime ASC )\n\
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
       \CREATE TABLE IF NOT EXISTS Links (\n\
       \    workspaceId TEXT NOT NULL,\n\
       \    sourceId INTEGER NOT NULL,\n\
       \    targetId INTEGER NOT NULL,\n\
       \    FOREIGN KEY ( workspaceId ) REFERENCES Workspaces ( id ) ON DELETE CASCADE\n\
       \    FOREIGN KEY ( sourceId ) REFERENCES Pointers ( id ) ON DELETE CASCADE\n\
       \    FOREIGN KEY ( targetId ) REFERENCES Pointers ( id ) ON DELETE CASCADE\n\
       \    PRIMARY KEY ( workspaceId ASC, sourceId ASC )\n\
       \);"
    execute_ conn "\
       \CREATE TABLE IF NOT EXISTS Continuations (\n\
       \    workspaceId TEXT NOT NULL,\n\
       \    function INTEGER NOT NULL,\n\
       \    next TEXT NOT NULL,\n\
       \    FOREIGN KEY ( function ) REFERENCES Functions ( id ) ON DELETE CASCADE\n\
       \    PRIMARY KEY ( workspaceId ASC, function ASC )\n\
       \);"
    execute_ conn "\
       \CREATE TABLE IF NOT EXISTS ContinuationEnvironments (\n\
       \    workspaceId TEXT NOT NULL,\n\
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
       \    workspaceId TEXT NOT NULL,\n\
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
       \    processId TEXT NOT NULL,\n\
       \    varEnv TEXT NOT NULL,\n\
       \    funEnv TEXT NOT NULL,\n\
       \    workspaceId TEXT NOT NULL,\n\
       \    expression TEXT NOT NULL,\n\
       \    continuation TEXT NOT NULL,\n\
       \    FOREIGN KEY ( workspaceId ) REFERENCES Workspaces ( id ) ON DELETE CASCADE\n\
       \);"
    execute_ conn "\
       \CREATE TABLE IF NOT EXISTS RunQueue (\n\
       \    processId TEXT PRIMARY KEY\n\
       \);"
    execute_ conn "\
       \CREATE TABLE IF NOT EXISTS Sessions (\n\
       \    sessionId TEXT PRIMARY KEY ASC\n\
       \);"
    execute_ conn "\
       \CREATE TABLE IF NOT EXISTS SessionProcesses (\n\
       \    sessionId TEXT NOT NULL,\n\
       \    processId TEXT NOT NULL,\n\
       \    FOREIGN KEY ( sessionId ) REFERENCES Sessions ( sessionId ) ON DELETE CASCADE\n\
       \    PRIMARY KEY ( sessionId ASC, processId ASC )\n\
       \);"
    execute_ conn "\
       \CREATE TABLE IF NOT EXISTS Primitives (\n\
       \    id INTEGER PRIMARY KEY ASC,\n\
       \    pattern TEXT NOT NULL,\n\
       \    body TEXT NOT NULL\n\
       \);"

