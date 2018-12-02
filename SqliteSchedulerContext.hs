{-# LANGUAGE OverloadedStrings #-}
module SqliteSchedulerContext ( makeSqliteSchedulerContext ) where
import Data.Int ( Int64 ) -- base
import qualified Data.Map as M -- containers
import Data.Text ( Text ) -- text
import Database.SQLite.Simple ( Connection, Only(..), NamedParam(..), query, query_, execute, executeMany, executeNamed, lastInsertRowId ) -- sqlite-simple

import Command ( Command(..), commandToBuilder )
import DataModel ( LogicalTime )
import Message ( Message(..), Pointer, PointerEnvironment, PointerRemapping,
                 singleLayer, normalizeMessage, generalizeMessage, instantiatePattern,
                 messageToBuilder, messageToBuilderDB, parseMessageUnsafe, parseMessageUnsafe' )
import Scheduler ( SchedulerContext(..) )
import Time ( Time(..) )
import Util ( toText )
import Workspace ( Workspace(..), WorkspaceId )

makeSqliteSchedulerContext :: Connection -> IO (SchedulerContext Connection)
makeSqliteSchedulerContext conn = return $
    SchedulerContext {
        createInitialWorkspace = createInitialWorkspaceSqlite conn,
        createWorkspace = createWorkspaceSqlite conn,
        sendAnswer = sendAnswerSqlite conn,
        sendMessage = sendMessageSqlite conn,
        expandPointer = expandPointerSqlite conn,
        getWorkspace = getWorkspaceSqlite conn,
        getNextWorkspace = getNextWorkspaceSqlite conn,
        labelMessage = labelMessageSqlite conn,
        normalize = insertMessagePointers conn,
        generalize = insertGeneralizedMessagePointers conn,
        instantiate = insertInstantiatedPatternPointers conn,
        singleLayerMatch = singleLayerMatchSqlite conn,
        dereference = dereferenceSqlite conn,
        extraContent = conn
    }

-- TODO: Bulkify this.
dereferenceSqlite :: Connection -> Pointer -> IO Message
dereferenceSqlite conn ptr = do
    [Only t] <- query conn "SELECT content FROM Pointers WHERE id = ? LIMIT 1" (Only ptr)
    return $! parseMessageUnsafe' ptr t

-- TODO: Can probably add a cache (using the MessageTrie) to avoid creating pointers unnecessarily,
-- for most of the operations that insert into Pointers, though some should not use the cache regardless.

-- Normalize the Message, write the new pointers to the database, then return the normalized message.
insertMessagePointers :: Connection -> Message -> IO Message
insertMessagePointers conn msg = do
    -- TODO: This is definitely a race condition.
    [Only lastPointerId] <- query_ conn "SELECT MAX(id) FROM Pointers"
    let (pEnv, normalizedMsg) = normalizeMessage (maybe 0 succ lastPointerId) msg
    executeMany conn "INSERT INTO Pointers (id, content) VALUES (?, ?)" (M.assocs (fmap (toText . messageToBuilderDB) pEnv))
    return normalizedMsg

insertGeneralizedMessagePointers :: Connection -> Message -> IO Message
insertGeneralizedMessagePointers conn msg = do
    -- TODO: This is definitely a race condition.
    [Only lastPointerId] <- query_ conn "SELECT MAX(id) FROM Pointers"
    let (mapping, generalizedMsg) = generalizeMessage (maybe 0 succ lastPointerId) msg
    executeMany conn "INSERT INTO Pointers (id, content) SELECT ?, o.content FROM Pointers o WHERE o.id = ?" (M.assocs mapping)
    return generalizedMsg

insertInstantiatedPatternPointers :: Connection -> PointerEnvironment -> Message -> IO (PointerRemapping, Message)
insertInstantiatedPatternPointers conn env msg = do
    -- TODO: This is definitely a race condition.
    [Only lastPointerId] <- query_ conn "SELECT MAX(id) FROM Pointers"
    let (pEnv, mapping, instantiatedPattern) = instantiatePattern (maybe 0 succ lastPointerId) env msg
    executeMany conn "INSERT INTO Pointers (id, content) VALUES (?, ?)" (M.assocs (fmap (toText . messageToBuilderDB) pEnv))
    return (mapping, instantiatedPattern)

labelMessageSqlite :: Connection -> Message -> IO Message
labelMessageSqlite conn msg@(Structured ms) = do
    execute conn "INSERT INTO Pointers (content) VALUES (?)" [toText (messageToBuilderDB msg)]
    p <- fromIntegral <$> lastInsertRowId conn
    return (LabeledStructured p ms)
labelMessageSqlite conn msg = do
    execute conn "INSERT INTO Pointers (content) VALUES (?)" [toText (messageToBuilderDB msg)]
    p <- fromIntegral <$> lastInsertRowId conn
    return (LabeledStructured p [msg])

singleLayerMatchSqlite :: Connection -> Message -> IO (PointerEnvironment, Message)
singleLayerMatchSqlite conn msg = do
    -- TODO: This is definitely a race condition.
    [Only lastPointerId] <- query_ conn "SELECT MAX(id) FROM Pointers"
    let (pEnv, pattern) = singleLayer (maybe 0 succ lastPointerId) msg
    executeMany conn "INSERT INTO Pointers (id, content) VALUES (?, ?)" (M.assocs (fmap (toText . messageToBuilderDB) pEnv))
    -- pattern <- insertGeneralizedMessagePointers conn pattern -- TODO: Do this?
    return (pEnv, pattern)

insertCommand :: Connection -> WorkspaceId -> Command -> IO ()
insertCommand conn workspaceId cmd = do
    mt <- query conn "SELECT localTime FROM Commands WHERE workspaceId = ? ORDER BY localTime DESC LIMIT 1" (Only workspaceId)
    let t = case mt of [] -> 0; [Only t'] -> t'+1
    executeNamed conn "INSERT INTO Commands (workspaceId, localTime, command) VALUES (:workspace, :time, :cmd)" [
                        ":workspace" := workspaceId,
                        ":time" := (t :: Int64),
                        ":cmd" := toText (commandToBuilder cmd)] -- TODO: Use normalized Messages here too?

createInitialWorkspaceSqlite :: Connection -> IO WorkspaceId
createInitialWorkspaceSqlite conn = do
    executeNamed conn "INSERT INTO Workspaces (logicalTime, parentWorkspaceId, question) VALUES (:time, :parent, :question)" [
                        ":time" := (0 :: LogicalTime),
                        ":parent" := (Nothing :: Maybe WorkspaceId),
                        ":question" := ("What is your question?" :: Text)]
    lastInsertRowId conn

createWorkspaceSqlite :: Connection -> Bool -> Workspace -> Message -> IO WorkspaceId
createWorkspaceSqlite conn doNormalize ws msg = do
    let workspaceId = identity ws
    msg' <- if doNormalize then insertMessagePointers conn msg else return msg
    executeNamed conn "INSERT INTO Workspaces (logicalTime, parentWorkspaceId, question) VALUES (:time, :parent, :question)" [
                        ":time" := (0 :: LogicalTime), -- TODO
                        ":parent" := Just workspaceId,
                        ":question" := toText (messageToBuilder msg')]
    newWorkspaceId <- lastInsertRowId conn
    insertCommand conn workspaceId (Ask msg)
    return newWorkspaceId

sendAnswerSqlite :: Connection -> Bool -> Workspace -> Message -> IO ()
sendAnswerSqlite conn doNormalize ws msg = do
    let workspaceId = identity ws
    msg' <- if doNormalize then insertMessagePointers conn msg else return msg
    executeNamed conn "INSERT INTO Answers (workspaceId, logicalTimeAnswered, answer) VALUES (:workspace, :time, :answer)" [
                        ":workspace" := workspaceId,
                        ":time" := (0 :: LogicalTime), -- TODO
                        ":answer" := toText (messageToBuilder msg')]
    insertCommand conn workspaceId (Reply msg)

sendMessageSqlite :: Connection -> Bool -> Workspace -> WorkspaceId -> Message -> IO ()
sendMessageSqlite conn doNormalize ws tgtId msg = do
    let srcId = identity ws
    msg' <- if doNormalize then insertMessagePointers conn msg else return msg
    executeNamed conn "INSERT INTO Messages (sourceWorkspaceId, targetWorkspaceId, logicalTimeSent, content) VALUES (:source, :target, :time, :content)" [
                        ":source" := srcId,
                        ":target" := tgtId,
                        ":time" := (0 :: LogicalTime), -- TODO
                        ":content" := toText (messageToBuilder msg')]
    insertCommand conn srcId (Send (fromIntegral tgtId) msg)

-- TODO: Bulkify this.
expandPointerSqlite :: Connection -> Workspace -> Pointer -> IO ()
expandPointerSqlite conn ws ptr = do
    let workspaceId = identity ws
    executeNamed conn "INSERT OR IGNORE INTO ExpandedPointers (workspaceId, pointerId, logicalTimeExpanded) VALUES (:workspace, :pointer, :time)" [
                        ":workspace" := workspaceId,
                        ":pointer" := ptr,
                        ":time" := (0 :: LogicalTime)] -- TODO
    insertCommand conn workspaceId (View ptr)

-- TODO: Maybe maintain a cache of workspaces.
getWorkspaceSqlite :: Connection -> WorkspaceId -> IO Workspace
getWorkspaceSqlite conn workspaceId = do
    -- TODO: Maybe use a transaction.
    [(p, t, q)] <- query conn "SELECT parentWorkspaceId, logicalTime, question FROM Workspaces WHERE id = ? ORDER BY logicalTime DESC LIMIT 1" (Only workspaceId)
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
        parentId = p,
        question = parseMessageUnsafe q,
        subQuestions = map (\(q, ma) -> (parseMessageUnsafe q, fmap parseMessageUnsafe ma)) subquestions,
        messageHistory = map (\(Only m) -> parseMessageUnsafe m) messages,
        expandedPointers = M.fromList $ map (\(p, m) -> (p, parseMessageUnsafe' p m)) expanded,
        time = Time t }

getNextWorkspaceSqlite :: Connection -> IO (Maybe WorkspaceId)
getNextWorkspaceSqlite conn = do
    -- TODO: How we order determines what workspace we're going to schedule next.
    -- This gets a workspace that doesn't currently have an answer. TODO: For now, want the deepest (which is the newest) one...
    result <- query conn "SELECT w.id \
                         \FROM Workspaces w \
                         \WHERE NOT EXISTS(SELECT * FROM Answers a WHERE a.workspaceId = w.id) ORDER BY w.id DESC LIMIT 1" ()
    case result of
        [] -> return Nothing
        [Only wId] -> return (Just wId)
