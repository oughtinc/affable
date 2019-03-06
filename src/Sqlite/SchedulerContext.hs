{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module Sqlite.SchedulerContext ( makeSqliteSchedulerContext ) where
import Control.Exception ( bracket_ ) -- base
import Data.Int ( Int64 ) -- base
import Data.IORef ( IORef, newIORef, writeIORef, readIORef ) -- base
import qualified Data.Map as M -- containers
import Data.Text ( Text ) -- text
import Database.SQLite.Simple ( Connection, Only(..), NamedParam(..),
                                query, query_, execute, execute_, executeMany, executeNamed, lastInsertRowId, withTransaction ) -- sqlite-simple

import Command ( Command(..), commandToBuilder )
import Message ( Message(..), Pointer, PointerEnvironment, PointerRemapping,
                 messageToBuilder, messageToBuilderDB, parseMessageUnsafe, parseMessageUnsafe', parseMessageUnsafeDB )
import Scheduler ( SchedulerContext(..), Event, UserId, SessionId, SyncFunc,
                   autoUserId, userIdToBuilder, sessionIdToBuilder, newSessionId, workspaceToMessage, eventMessage, renumberEvent )
import Time ( Time(..), LogicalTime )
import Util ( toText, Counter, newCounter, increment, Queue, newQueue, enqueueAsync, enqueueSync )
import Workspace ( Workspace(..), WorkspaceId, newWorkspaceId, workspaceIdFromText, workspaceIdToBuilder )

makeSqliteSchedulerContext :: Connection -> IO (SchedulerContext (Connection, Queue))
makeSqliteSchedulerContext conn = do
    q <- newQueue
    [Only t] <- enqueueSync q $ query_ conn "SELECT COUNT(*) FROM Commands"
    c <- newCounter t
    nestedRef <- newIORef False
    let sync action = do
            nested <- readIORef nestedRef
            if nested then action else enqueueSync q (bracket_ (writeIORef nestedRef True) (writeIORef nestedRef False) action)
    return $
        SchedulerContext {
            doAtomically = sync,
            createInitialWorkspace = createInitialWorkspaceSqlite sync q c conn,
            newSession = newSessionSqlite sync q c conn,
            createWorkspace = createWorkspaceSqlite sync q c conn,
            sendAnswer = sendAnswerSqlite sync q c conn,
            sendMessage = sendMessageSqlite sync q c conn,
            expandPointer = expandPointerSqlite sync q c conn,
            nextPointer = nextPointerSqlite sync q c conn,
            createPointers = createPointersSqlite sync q c conn,
            remapPointers = remapPointersSqlite sync q c conn,
            pendingQuestions = pendingQuestionsSqlite sync q c conn,
            getWorkspace = getWorkspaceSqlite sync q c conn,
            allWorkspaces = allWorkspacesSqlite sync q c conn,
            getNextWorkspace = getNextWorkspaceSqlite sync q c conn,
            dereference = dereferenceSqlite sync q c conn,
            reifyWorkspace = reifyWorkspaceSqlite sync q c conn,
            extraContent = (conn, q)
        }

-- NOT CACHEABLE
reifyWorkspaceSqlite :: SyncFunc -> Queue -> Counter -> Connection -> WorkspaceId -> IO Message
reifyWorkspaceSqlite sync q c conn workspaceId = do
    let !wsIdText = toText (workspaceIdToBuilder workspaceId)
    workspaces <- sync $ do
        withTransaction conn $ do
            execute_ conn "CREATE TEMP TABLE IF NOT EXISTS Descendants ( id INTEGER PRIMARY KEY ASC )"
            execute_ conn "DELETE FROM Descendants"
            executeNamed conn "WITH RECURSIVE ds(id) AS ( \
                              \     VALUES (:root) \
                              \ UNION ALL \
                              \     SELECT w.id FROM Workspaces w INNER JOIN ds ON w.parentWorkspaceId = ds.id \
                              \) INSERT INTO Descendants ( id ) SELECT id FROM ds" [":root" := wsIdText]
            workspaces <- query_ conn "SELECT id, parentWorkspaceId, logicalTime, questionAsAnswered \
                                      \FROM Workspaces WHERE id IN (SELECT id FROM Descendants)"
            messages <- query_ conn "SELECT targetWorkspaceId, content \
                                    \FROM Messages \
                                    \WHERE targetWorkspaceId IN (SELECT id FROM Descendants) \
                                    \ORDER BY id ASC"
            subquestions <- query_ conn "SELECT p.id, q.id, q.questionAsAsked, a.answer \
                                        \FROM Workspaces p \
                                        \INNER JOIN Workspaces q ON q.parentWorkspaceId = p.id \
                                        \LEFT OUTER JOIN Answers a ON q.id = a.workspaceId \
                                        \WHERE p.id IN (SELECT id FROM Descendants) \
                                        \ORDER BY p.id ASC, q.logicalTime DESC"
            {-
            expanded <- query_ conn "SELECT workspaceId, pointerId, content \
                                    \FROM ExpandedPointers e \
                                    \INNER JOIN Pointers p ON e.pointerId = p.id \
                                    \WHERE workspaceId IN (SELECT id FROM Descendants)"
            -}
            let messageMap = M.fromListWith (++) $ map (\(i, m) -> (workspaceIdFromText i, [parseMessageUnsafe m])) messages
                subquestionsMap = M.fromListWith (++) $
                                    map (\(i, qId, q, ma) ->
                                            (workspaceIdFromText i, [(workspaceIdFromText qId, parseMessageUnsafe q, fmap parseMessageUnsafeDB ma)])) subquestions
                expandedMap = M.empty -- M.fromListWith M.union $ map (\(i, p, m) -> (i, M.singleton p (parseMessageUnsafe' p m))) expanded
            return $ M.fromList $ map (\(i, p, t, q) -> let !wsId = workspaceIdFromText i
                                                        in (wsId, Workspace {
                                                                    identity = wsId,
                                                                    parentId = workspaceIdFromText <$> p,
                                                                    question = parseMessageUnsafeDB q,
                                                                    subQuestions = maybe [] id $ M.lookup wsId subquestionsMap,
                                                                    messageHistory = maybe [] id $ M.lookup wsId messageMap,
                                                                    expandedPointers = maybe M.empty id $ M.lookup wsId expandedMap,
                                                                    time = Time t })) workspaces
    return (workspaceToMessage workspaces workspaceId)

-- TODO: Bulkify this.
-- CACHEABLE
dereferenceSqlite :: SyncFunc -> Queue -> Counter -> Connection -> Pointer -> IO Message
dereferenceSqlite sync q c conn ptr = do
    sync $ do
        [Only t] <- query conn "SELECT content FROM Pointers WHERE id = ? LIMIT 1" (Only ptr)
        return $! parseMessageUnsafe' ptr t

insertCommand :: SyncFunc -> Queue -> Counter -> Connection -> UserId -> WorkspaceId -> Command -> IO ()
insertCommand sync q c conn userId workspaceId cmd = do
    let !cmdText = toText (commandToBuilder cmd)
    let !wsIdText = toText (workspaceIdToBuilder workspaceId)
    enqueueAsync q $ do
        mt <- query conn "SELECT commandTime FROM Commands WHERE workspaceId = ? ORDER BY commandTime DESC LIMIT 1" (Only wsIdText)
        let t = case mt of [] -> 0; [Only t'] -> t'+1
        executeNamed conn "INSERT INTO Commands (workspaceId, commandTime, userId, command) VALUES (:workspace, :time, :userId, :cmd)" [
                            ":workspace" := wsIdText,
                            ":time" := (t :: Int64),
                            ":userId" := toText (userIdToBuilder userId),
                            ":cmd" := cmdText]

createInitialWorkspaceSqlite :: SyncFunc -> Queue -> Counter -> Connection -> IO WorkspaceId
createInitialWorkspaceSqlite sync q c conn = do
    workspaceId <- newWorkspaceId
    let !wsIdText = toText (workspaceIdToBuilder workspaceId)
    t <- increment c
    let msg = Text "What is your question?"
    enqueueAsync q $ do
        withTransaction conn $ do
            [Only lastPointerId] <- query_ conn "SELECT MAX(id) FROM Pointers"
            let !p = maybe 0 succ lastPointerId
            let msg' = LabeledStructured p [msg]
            let !msgText = toText (messageToBuilder msg)
                !msgText' = toText (messageToBuilder msg')
            executeNamed conn "INSERT INTO Workspaces (id, logicalTime, parentWorkspaceId, questionAsAsked, questionAsAnswered) \
                              \VALUES (:id, :time, :parent, :questionAsAsked, :questionAsAnswered)" [
                                ":id" := wsIdText,
                                ":time" := (t :: LogicalTime),
                                ":parent" := (Nothing :: Maybe Text),
                                ":questionAsAsked" := msgText,
                                ":questionAsAnswered" := msgText']
    insertCommand sync q c conn autoUserId workspaceId (Ask msg)
    return workspaceId

newSessionSqlite :: SyncFunc -> Queue -> Counter -> Connection -> Maybe SessionId -> IO SessionId
newSessionSqlite sync q c conn Nothing = do
    sessionId <- newSessionId
    newSessionSqlite sync q c conn (Just sessionId)
newSessionSqlite sync q c conn (Just sessionId) = do
    enqueueAsync q $
        execute conn "INSERT OR IGNORE INTO Sessions (sessionId) VALUES (?)" (Only (toText (sessionIdToBuilder sessionId)))
    return sessionId

createWorkspaceSqlite :: SyncFunc -> Queue -> Counter -> Connection -> UserId -> WorkspaceId -> Message -> Message -> IO WorkspaceId
createWorkspaceSqlite sync q c conn userId workspaceId qAsAsked qAsAnswered = do
    let !qAsAskedText = toText (messageToBuilder qAsAsked)
        !qAsAnsweredText = toText (messageToBuilder qAsAnswered)
    wsId <- newWorkspaceId
    let !workspaceIdText = toText (workspaceIdToBuilder workspaceId)
    let !wsIdText = toText (workspaceIdToBuilder wsId)
    t <- increment c
    enqueueAsync q $ do
        executeNamed conn "INSERT INTO Workspaces (id, logicalTime, parentWorkspaceId, questionAsAsked, questionAsAnswered) \
                          \VALUES (:id, :time, :parent, :asAsked, :asAnswered)" [
                            ":id" := wsIdText,
                            ":time" := (t :: LogicalTime),
                            ":parent" := Just workspaceIdText,
                            ":asAsked" := qAsAskedText,
                            ":asAnswered" := qAsAnsweredText]
    insertCommand sync q c conn userId workspaceId (Ask qAsAsked)
    return wsId

sendAnswerSqlite :: SyncFunc -> Queue -> Counter -> Connection -> UserId -> WorkspaceId -> Message -> IO ()
sendAnswerSqlite sync q c conn userId workspaceId msg = do
    let !msgText = toText (messageToBuilder msg)
    let !wsIdText = toText (workspaceIdToBuilder workspaceId)
    t <- increment c
    enqueueAsync q $ do
        -- TODO: XXX If we revisit, and thus change an answer, this will need to be an INSERT OR REPLACE or we'll need to start
        -- actually using this time parameter. If this is all that is changed, then we'll get a model of edits where we see
        -- the following questions upon return, possibly referring to pointers in an answer that no longer exist.
        executeNamed conn "INSERT OR REPLACE INTO Answers (workspaceId, logicalTimeAnswered, answer) VALUES (:workspace, :time, :answer)" [
                            ":workspace" := wsIdText,
                            ":time" := (t :: LogicalTime),
                            ":answer" := msgText]
    insertCommand sync q c conn userId workspaceId (Reply msg)

sendMessageSqlite :: SyncFunc -> Queue -> Counter -> Connection -> UserId -> WorkspaceId -> WorkspaceId -> Message -> IO ()
sendMessageSqlite sync q c conn userId srcId tgtId msg = do
    let !msgText = toText (messageToBuilder msg)
    t <- increment c
    enqueueAsync q $ do
        executeNamed conn "INSERT INTO Messages (sourceWorkspaceId, targetWorkspaceId, logicalTimeSent, content) VALUES (:source, :target, :time, :content)" [
                            ":source" := toText (workspaceIdToBuilder srcId),
                            ":target" := toText (workspaceIdToBuilder tgtId),
                            ":time" := (t :: LogicalTime),
                            ":content" := msgText]
    insertCommand sync q c conn userId srcId (Send tgtId msg)

-- TODO: Bulkify this.
expandPointerSqlite :: SyncFunc -> Queue -> Counter -> Connection -> UserId -> WorkspaceId -> Pointer -> IO ()
expandPointerSqlite sync q c conn userId workspaceId ptr = do
    let !wsIdText = toText (workspaceIdToBuilder workspaceId)
    t <- increment c
    enqueueAsync q $ do
        executeNamed conn "INSERT OR IGNORE INTO ExpandedPointers (workspaceId, pointerId, logicalTimeExpanded) VALUES (:workspace, :pointer, :time)" [
                            ":workspace" := wsIdText,
                            ":pointer" := ptr,
                            ":time" := (t :: LogicalTime)]
    insertCommand sync q c conn userId workspaceId (View ptr)

nextPointerSqlite :: SyncFunc -> Queue -> Counter -> Connection -> IO Pointer
nextPointerSqlite sync q c conn = do
    sync $ do
        [Only lastPointerId] <- query_ conn "SELECT MAX(id) FROM Pointers"
        return $! maybe 0 succ lastPointerId

createPointersSqlite :: SyncFunc -> Queue -> Counter -> Connection -> PointerEnvironment -> IO ()
createPointersSqlite sync q c conn env = do
    enqueueAsync q $
        executeMany conn "INSERT INTO Pointers (id, content) VALUES (?, ?)" (M.assocs (fmap (toText . messageToBuilderDB) env))

remapPointersSqlite :: SyncFunc -> Queue -> Counter -> Connection -> PointerRemapping -> IO ()
remapPointersSqlite sync q c conn mapping = do
    enqueueAsync q $
        executeMany conn "INSERT INTO Pointers (id, content) \
                         \SELECT ?, content \
                         \FROM Pointers \
                         \WHERE id = ?" (M.assocs mapping)

-- NOT CACHEABLE
pendingQuestionsSqlite :: SyncFunc -> Queue -> Counter -> Connection -> WorkspaceId -> IO [WorkspaceId]
pendingQuestionsSqlite sync q c conn workspaceId = do
    let !wsIdText = toText (workspaceIdToBuilder workspaceId)
    sync $ do
        subquestions <- query conn "SELECT w.id \
                                   \FROM Workspaces w \
                                   \LEFT OUTER JOIN Answers a ON a.workspaceId = w.id \
                                   \WHERE w.parentWorkspaceId = ? \
                                   \  AND a.answer IS NULL ORDER BY logicalTime ASC" (Only wsIdText)
        return $ map (\(Only qId) -> workspaceIdFromText qId) subquestions

-- TODO: Maybe maintain a cache of workspaces.
-- NOT CACHEABLE but the components should be. Cacheable if answered, for now at least.
getWorkspaceSqlite :: SyncFunc -> Queue -> Counter -> Connection -> WorkspaceId -> IO Workspace
getWorkspaceSqlite sync q c conn workspaceId = do
    let !wsIdText = toText (workspaceIdToBuilder workspaceId)
    sync $ do
        withTransaction conn $ do
            [(p, t, q)] <- query conn "SELECT parentWorkspaceId, logicalTime, questionAsAnswered \
                                      \FROM Workspaces \
                                      \WHERE id = ? \
                                      \ORDER BY logicalTime DESC LIMIT 1" (Only wsIdText)
            messages <- query conn "SELECT content FROM Messages WHERE targetWorkspaceId = ?" (Only wsIdText) -- TODO: ORDER
            subquestions <- query conn "SELECT w.id, w.questionAsAsked, a.answer \
                                       \FROM Workspaces w \
                                       \LEFT OUTER JOIN Answers a ON w.id = a.workspaceId \
                                       \WHERE w.parentWorkspaceId = ? \
                                       \ORDER BY w.logicalTime ASC" (Only wsIdText)
            expanded <- query conn "SELECT pointerId, content \
                                   \FROM ExpandedPointers e \
                                   \INNER JOIN Pointers p ON e.pointerId = p.id \
                                   \WHERE e.workspaceId = ?" (Only wsIdText)
            return $ Workspace {
                identity = workspaceId,
                parentId = workspaceIdFromText <$> p,
                question = parseMessageUnsafeDB q,
                subQuestions = map (\(qId, q, ma) -> (workspaceIdFromText qId, parseMessageUnsafe q, fmap parseMessageUnsafeDB ma)) subquestions,
                messageHistory = map (\(Only m) -> parseMessageUnsafe m) messages,
                expandedPointers = M.fromList $ map (\(p, m) -> (p, parseMessageUnsafe' p m)) expanded,
                time = Time t }

-- NOT CACHEABLE
allWorkspacesSqlite :: SyncFunc -> Queue -> Counter -> Connection -> IO (M.Map WorkspaceId Workspace)
allWorkspacesSqlite sync q c conn = do
    sync $ do
        withTransaction conn $ do
            workspaces <- query_ conn "SELECT id, parentWorkspaceId, logicalTime, questionAsAnswered \
                                      \FROM Workspaces"
            messages <- query_ conn "SELECT targetWorkspaceId, content FROM Messages" -- TODO: ORDER
            subquestions <- query_ conn "SELECT p.id, q.id, q.questionAsAsked, a.answer \
                                        \FROM Workspaces p \
                                        \INNER JOIN Workspaces q ON q.parentWorkspaceId = p.id \
                                        \LEFT OUTER JOIN Answers a ON q.id = a.workspaceId \
                                        \ORDER BY p.id ASC, q.logicalTime DESC"
            expanded <- query_ conn "SELECT workspaceId, pointerId, content \
                                    \FROM ExpandedPointers e \
                                    \INNER JOIN Pointers p ON e.pointerId = p.id"
            let messageMap = M.fromListWith (++) $
                                map (\(i, m) -> (workspaceIdFromText i, [parseMessageUnsafe m])) messages
                subquestionsMap = M.fromListWith (++) $
                                    map (\(i, qId, q, ma) ->
                                        (workspaceIdFromText i, [(workspaceIdFromText qId, parseMessageUnsafe q, fmap parseMessageUnsafeDB ma)])) subquestions
                expandedMap = M.fromListWith M.union $
                                map (\(i, p, m) -> (workspaceIdFromText i, M.singleton p (parseMessageUnsafe' p m))) expanded
            return $ M.fromList $ map (\(i, p, t, q) -> let !wsId = workspaceIdFromText i
                                                        in (wsId, Workspace {
                                                                    identity = wsId,
                                                                    parentId = workspaceIdFromText <$> p,
                                                                    question = parseMessageUnsafeDB q,
                                                                    subQuestions = maybe [] id $ M.lookup wsId subquestionsMap,
                                                                    messageHistory = maybe [] id $ M.lookup wsId messageMap,
                                                                    expandedPointers = maybe M.empty id $ M.lookup wsId expandedMap,
                                                                    time = Time t })) workspaces

-- NOT CACHEABLE
getNextWorkspaceSqlite :: SyncFunc -> Queue -> Counter -> Connection -> IO (Maybe WorkspaceId)
getNextWorkspaceSqlite sync q c conn = do
    sync $ do
        -- This gets a workspace that doesn't currently have an answer.
        result <- query_ conn "SELECT w.id \
                              \FROM Workspaces w \
                              \WHERE NOT EXISTS(SELECT * FROM Answers a WHERE a.workspaceId = w.id) ORDER BY w.logicalTime DESC LIMIT 1"
        case result of
            [] -> return Nothing
            [Only workspaceId] -> return (Just $ workspaceIdFromText workspaceId)
