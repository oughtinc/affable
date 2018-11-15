{-# LANGUAGE OverloadedStrings #-}
module SqliteSchedulerContext ( makeSqliteSchedulerContext ) where
import Data.Int ( Int64 ) -- base
import qualified Data.Map as M -- containers
import Data.Text ( Text ) -- text
import Database.SQLite.Simple ( Connection, Query, Only(..), NamedParam(..), query, execute, execute_, executeNamed, lastInsertRowId ) -- sqlite-simple

import Command ( Command(..), commandToBuilder )
import DataModel ( LogicalTime )
import Message
import Scheduler ( SchedulerContext(..) )
import Time ( Time(..) )
import Util ( toText )
import Workspace ( Workspace(..), WorkspaceId )

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

insertCommand :: Connection -> WorkspaceId -> Command -> IO ()
insertCommand conn workspaceId cmd = do
    mt <- query conn "SELECT localTime FROM Commands WHERE workspaceId = ? ORDER BY localTime DESC LIMIT 1" (Only workspaceId)
    let t = case mt of [] -> 0; [Only t'] -> t'+1
    executeNamed conn "INSERT INTO Commands (workspaceId, localTime, command) VALUES (:workspace, :time, :cmd)" [
                        ":workspace" := workspaceId,
                        ":time" := (t :: Int64),
                        ":cmd" := toText (commandToBuilder cmd)]

createWorkspaceSqlite :: Connection -> Workspace -> Message -> IO WorkspaceId
createWorkspaceSqlite conn ws msg = do
    let workspaceId = identity ws
    executeNamed conn "INSERT INTO Workspaces (logicalTime, parentWorkspaceId, question) VALUES (:time, :parent, :question)" [
                        ":time" := (0 :: LogicalTime), -- TODO
                        ":parent" := Just workspaceId,
                        ":question" := toText (messageToBuilder msg)]
    newWorkspaceId <- lastInsertRowId conn
    insertCommand conn workspaceId (Ask msg)
    return newWorkspaceId

sendAnswerSqlite :: Connection -> Workspace -> Message -> IO ()
sendAnswerSqlite conn ws msg = do
    let workspaceId = identity ws
    executeNamed conn "INSERT INTO Answers (workspaceId, logicalTimeAnswered, answer) VALUES (:workspace, :time, :answer)" [
                        ":workspace" := workspaceId,
                        ":time" := (0 :: LogicalTime), -- TODO
                        ":answer" := toText (messageToBuilder msg)]
    insertCommand conn workspaceId (Reply msg)

sendMessageSqlite :: Connection -> Workspace -> WorkspaceId -> Message -> IO ()
sendMessageSqlite conn ws tgtId msg = do
    let srcId = identity ws
    executeNamed conn "INSERT INTO Messages (sourceWorkspaceId, targetWorkspaceId, logicalTimeAnswered, content) VALUES (:source, :target, :time, :content)" [
                        ":source" := srcId,
                        ":target" := tgtId,
                        ":time" := (0 :: LogicalTime), -- TODO
                        ":content" := toText (messageToBuilder msg)]
    insertCommand conn srcId (Send (fromIntegral tgtId) msg)

expandPointerSqlite :: Connection -> Workspace -> Pointer -> IO ()
expandPointerSqlite conn ws ptr = do
    let workspaceId = identity ws
    executeNamed conn "INSERT INTO ExpandedPointers (workspaceId, pointerId, logicalTimeExpanded) VALUES (:workspace, :pointer, :time)" [
                        ":workspace" := workspaceId,
                        ":pointer" := ptr,
                        ":time" := (0 :: LogicalTime)] -- TODO
    insertCommand conn workspaceId (View ptr)

getWorkspaceSqlite :: Connection -> WorkspaceId -> IO Workspace
getWorkspaceSqlite conn workspaceId = do
    -- TODO: Maybe use a transaction.
    [(t, q)] <- query conn "SELECT logicalTime, question FROM Workspaces WHERE id = ? ORDER BY logicalTime DESC LIMIT 1" (Only workspaceId)
    messages <- query conn "SELECT content FROM Messages WHERE targetWorkspaceId = ?" (Only workspaceId)
    subquestions <- query conn "SELECT w.question, a.answer \
                               \FROM Workspaces w \
                               \LEFT OUTER JOIN Answers a ON w.id = a.workspaceId \
                               \WHERE w.parentWorkspaceId = ?"  (Only workspaceId)
    expanded <- query conn "SELECT pointerId, content \
                           \FROM ExpandedPointers e \
                           \INNER JOIN Pointers p ON e.pointerId = p.id \
                           \WHERE e.workspaceId = ?" (Only workspaceId)
    return $ Workspace {
        identity = workspaceId,
        question = parseMessageUnsafe q,
        subQuestions = map (\(q, ma) -> (parseMessageUnsafe q, fmap parseMessageUnsafe ma)) subquestions,
        messageHistory = map (\(Only m) -> parseMessageUnsafe m) messages,
        expandedPointers = M.fromList $ map (\(p, m) -> (p, parseMessageUnsafe m)) expanded,
        time = Time t }

getNextWorkspaceSqlite :: Connection -> IO (Maybe WorkspaceId)
getNextWorkspaceSqlite conn = do
    -- TODO: How we order determines what workspace we're going to schedule next.
    -- This gets a workspace that doesn't currently have an answer.
    result <- query conn "SELECT w.id FROM Workspaces w WHERE NOT EXISTS(SELECT * FROM Answers a WHERE a.workspaceId = w.id) ORDER BY w.id LIMIT 1" ()
    case result of
        [] -> return Nothing
        [Only wId] -> return (Just wId)
