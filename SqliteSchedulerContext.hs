{-# LANGUAGE OverloadedStrings #-}
module SqliteSchedulerContext ( makeSqliteSchedulerContext ) where
import Data.Text ( Text ) -- text
import Database.SQLite.Simple ( Connection, Query, Only(..), NamedParam(..), query, execute, execute_, executeNamed, lastInsertRowId ) -- sqlite-simple

import Command ( Command(..), commandToBuilder )
import DataModel ( WorkspaceRow(..), WorkspaceId, LogicalTime )
import Message
import Scheduler
import Util ( toText )
import Workspace ( Workspace )

makeSqliteSchedulerContext :: Connection -> IO (SchedulerContext Connection)
makeSqliteSchedulerContext conn = return $
    SchedulerContext {
        createWorkspace = createWorkspaceSqlite conn,
        sendAnswer = sendAnswerSqlite conn,
        sendMessage = sendMessageSqlite conn,
        expandPointer = expandPointerSqlite conn,
        getWorkspace = getWorkspaceSqlite conn,
        getNextWorkspace = getNextWorkspaceSqlite conn,
        extraContent = conn
    }

createWorkspaceSqlite :: Connection -> Maybe WorkspaceId -> Message -> IO ()
createWorkspaceSqlite conn mWorkspaceId msg = do
    executeNamed conn "INSERT INTO Workspaces (logicalTime, parentWorkspaceId, question) VALUES (:time, :parent, :question)" [
                        ":time" := (0 :: LogicalTime), 
                        ":parent" := mWorkspaceId, 
                        ":question" := toText (messageToBuilder msg)]
    case mWorkspaceId of -- TODO: Think about this.
        Nothing -> return ()
        Just wId -> executeNamed conn "INSERT INTO Commands (workspaceId, localTime, command) VALUES (:workspace, :time, :cmd)" [
                                        ":workspace" := wId,
                                        ":time" := (0 :: LogicalTime),
                                        ":cmd" := toText (commandToBuilder $ Ask msg)]

sendAnswerSqlite :: Connection -> WorkspaceId -> Message -> IO ()
sendAnswerSqlite conn workspaceId msg = do
    return () -- TODO

sendMessageSqlite :: Connection -> WorkspaceId -> Message -> IO ()
sendMessageSqlite conn workspaceId msg = do
    return () -- TODO

expandPointerSqlite :: Connection -> WorkspaceId -> Pointer -> IO ()
expandPointerSqlite conn workspaceId ptr = do
    return () -- TODO

getWorkspaceSqlite :: Connection -> WorkspaceId -> IO Workspace
getWorkspaceSqlite conn workspaceId = do
    return undefined -- TODO

getNextWorkspaceSqlite :: Connection -> IO (Maybe WorkspaceId)
getNextWorkspaceSqlite conn = do
    -- TODO: How we order determines what workspace we're going to schedule next.
    -- This gets a workspace that doesn't currently have an answer.
    result <- query conn "SELECT w.* FROM Workspaces w WHERE NOT EXISTS(SELECT * FROM Answers a WHERE a.workspaceId = w.id) ORDER BY w.id LIMIT 1" ()
    case result of
        [] -> return Nothing
        [Only wId] -> return (Just wId)
