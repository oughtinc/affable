{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module Postgres.SchedulerContext ( makePostgresSchedulerContext ) where
import Control.Concurrent ( myThreadId ) -- base
import Data.Int ( Int64 ) -- base
import qualified Data.Map as M -- containers
import Data.Text ( Text ) -- text
import Database.PostgreSQL.Simple ( Connection, Only(..),
                                    query, query_, execute, execute_, executeMany, withTransaction ) -- postgresql-simple

import Command ( Command(..), commandToBuilder )
import Message ( Message(..), Pointer, PointerEnvironment, PointerRemapping,
                 messageToBuilder, messageToBuilderDB, parseMessageUnsafe, parseMessageUnsafe', parseMessageUnsafeDB )
import Scheduler ( SchedulerContext(..), Event, UserId, SessionId, SyncFunc, AsyncFunc,
                   autoUserId, workspaceToMessage, eventMessage, renumberEvent, newSessionId )
import Time ( Time(..), LogicalTime )
import Util ( toText, Counter, newCounter, increment, Queue, enqueueAsync, enqueueSync )
import Workspace ( Workspace(..), WorkspaceId, newWorkspaceId )

makePostgresSchedulerContext :: Queue -> Connection -> IO (SchedulerContext (Connection, Queue))
makePostgresSchedulerContext q conn = do
    [Only t] <- enqueueSync q $ query_ conn "SELECT COUNT(*) FROM Commands"
    c <- newCounter t
    qThreadId <- enqueueSync q myThreadId
    let sync action = do
            tId <- myThreadId
            if qThreadId == tId then action -- If we're already on the Queue's thread, no need to enqueue.
                                else enqueueSync q (withTransaction conn action)
        async action = do
            tId <- myThreadId
            if qThreadId == tId then action -- If we're already on the Queue's thread, no need to enqueue.
                                else enqueueAsync q action
    return $
        SchedulerContext {
            doAtomically = sync,
            createInitialWorkspace = createInitialWorkspacePostgres sync async c conn,
            newSession = newSessionPostgres sync async c conn,
            createWorkspace = createWorkspacePostgres sync async c conn,
            sendAnswer = sendAnswerPostgres sync async c conn,
            sendMessage = sendMessagePostgres sync async c conn,
            expandPointer = expandPointerPostgres sync async c conn,
            nextPointer = nextPointerPostgres sync async c conn,
            createPointers = createPointersPostgres sync async c conn,
            remapPointers = remapPointersPostgres sync async c conn,
            pendingQuestions = pendingQuestionsPostgres sync async c conn,
            getWorkspace = getWorkspacePostgres sync async c conn,
            getNextWorkspace = getNextWorkspacePostgres sync async c conn,
            dereference = dereferencePostgres sync async c conn,
            reifyWorkspace = reifyWorkspacePostgres sync async c conn,
            extraContent = (conn, q)
        }

-- NOT CACHEABLE
reifyWorkspacePostgres :: SyncFunc -> AsyncFunc -> Counter -> Connection -> WorkspaceId -> IO Message
reifyWorkspacePostgres sync async c conn workspaceId = do
    workspaces <- sync $ do
        execute_ conn "CREATE TEMP TABLE IF NOT EXISTS Descendants ( id INTEGER PRIMARY KEY )"
        execute_ conn "DELETE FROM Descendants"
        execute conn "WITH RECURSIVE ds(id) AS ( \
                     \     VALUES (?) \
                     \ UNION ALL \
                     \     SELECT w.id FROM Workspaces w INNER JOIN ds ON w.parentWorkspaceId = ds.id \
                     \) INSERT INTO Descendants ( id ) SELECT id FROM ds" (Only workspaceId)
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
        let messageMap = M.fromListWith (++) $ map (\(i, m) -> (i, [parseMessageUnsafe m])) messages
            subquestionsMap = M.fromListWith (++) $ map (\(i, qId, q, ma) -> (i, [(qId, parseMessageUnsafe q, fmap parseMessageUnsafeDB ma)])) subquestions
            expandedMap = M.empty -- M.fromListWith M.union $ map (\(i, p, m) -> (i, M.singleton p (parseMessageUnsafe' p m))) expanded
        return $ M.fromList $ map (\(i, p, t, q) -> (i, Workspace {
                                                            identity = i,
                                                            parentId = p,
                                                            question = parseMessageUnsafeDB q,
                                                            subQuestions = maybe [] id $ M.lookup i subquestionsMap,
                                                            messageHistory = maybe [] id $ M.lookup i messageMap,
                                                            expandedPointers = maybe M.empty id $ M.lookup i expandedMap,
                                                            time = Time t })) workspaces
    return (workspaceToMessage workspaces workspaceId)

-- TODO: Bulkify this.
-- CACHEABLE
dereferencePostgres :: SyncFunc -> AsyncFunc -> Counter -> Connection -> Pointer -> IO Message
dereferencePostgres sync async c conn ptr = do
    sync $ do
        [Only t] <- query conn "SELECT content FROM Pointers WHERE id = ? LIMIT 1" (Only ptr)
        return $! parseMessageUnsafe' ptr t

insertCommand :: SyncFunc -> AsyncFunc -> Counter -> Connection -> UserId -> WorkspaceId -> Command -> IO ()
insertCommand sync async c conn userId workspaceId cmd = do
    let !cmdText = toText (commandToBuilder cmd)
    async $ do
        mt <- query conn "SELECT commandTime FROM Commands WHERE workspaceId = ? ORDER BY commandTime DESC LIMIT 1" (Only workspaceId)
        let t = case mt of [] -> 0; [Only t'] -> t'+1
        () <$ execute conn "INSERT INTO Commands (workspaceId, commandTime, userId, command) VALUES (?, ?, ?, ?)"
                            (workspaceId, t :: Int64, userId, cmdText)

createInitialWorkspacePostgres :: SyncFunc -> AsyncFunc -> Counter -> Connection -> Message -> IO WorkspaceId
createInitialWorkspacePostgres sync async c conn msg = do
    workspaceId <- newWorkspaceId
    t <- increment c
    let !msgText = toText (messageToBuilder msg)
    async $ do
        withTransaction conn $ do
            () <$ execute conn "INSERT INTO Workspaces (id, logicalTime, parentWorkspaceId, questionAsAsked, questionAsAnswered) \
                               \VALUES (?, ?, ?, ?, ?)"
                                 (workspaceId, t :: LogicalTime, Nothing :: Maybe WorkspaceId, msgText, msgText)
    insertCommand sync async c conn autoUserId workspaceId (Ask msg)
    return workspaceId

newSessionPostgres :: SyncFunc -> AsyncFunc -> Counter -> Connection -> Maybe SessionId -> IO SessionId
newSessionPostgres sync async c conn Nothing = do
    sessionId <- newSessionId
    newSessionPostgres sync async c conn (Just sessionId)
newSessionPostgres sync async c conn (Just sessionId) = do
    async $ do
        () <$ execute conn "INSERT INTO Sessions (sessionId) VALUES (?) ON CONFLICT DO NOTHING" (Only sessionId)
    return sessionId

createWorkspacePostgres :: SyncFunc -> AsyncFunc -> Counter -> Connection -> UserId -> WorkspaceId -> Message -> Message -> IO WorkspaceId
createWorkspacePostgres sync async c conn userId workspaceId qAsAsked qAsAnswered = do
    let !qAsAskedText = toText (messageToBuilder qAsAsked)
        !qAsAnsweredText = toText (messageToBuilder qAsAnswered)
    wsId <- newWorkspaceId
    t <- increment c
    async $ do
        () <$ execute conn "INSERT INTO Workspaces (id, logicalTime, parentWorkspaceId, questionAsAsked, questionAsAnswered) \
                           \VALUES (?, ?, ?, ?, ?)"
                            (wsId, t :: LogicalTime, Just workspaceId, qAsAskedText, qAsAnsweredText)
    insertCommand sync async c conn userId workspaceId (Ask qAsAsked)
    return wsId

sendAnswerPostgres :: SyncFunc -> AsyncFunc -> Counter -> Connection -> UserId -> WorkspaceId -> Message -> IO ()
sendAnswerPostgres sync async c conn userId workspaceId msg = do
    let !msgText = toText (messageToBuilder msg)
    t <- increment c
    async $ do
        () <$ execute conn "INSERT INTO Answers (workspaceId, logicalTimeAnswered, answer) VALUES (?, ?, ?) \
                           \ON CONFLICT(workspaceId) DO UPDATE SET logicalTimeAnswered = excluded.logicalTimeAnswered, answer = excluded.answer"
                            (workspaceId, t :: LogicalTime, msgText)
    insertCommand sync async c conn userId workspaceId (Reply msg)

sendMessagePostgres :: SyncFunc -> AsyncFunc -> Counter -> Connection -> UserId -> WorkspaceId -> WorkspaceId -> Message -> IO ()
sendMessagePostgres sync async c conn userId srcId tgtId msg = do
    let !msgText = toText (messageToBuilder msg)
    t <- increment c
    async $ do
        () <$ execute conn "INSERT INTO Messages (sourceWorkspaceId, targetWorkspaceId, logicalTimeSent, content) VALUES (?, ?, ?, ?)"
                            (srcId, tgtId, t :: LogicalTime, msgText)
    insertCommand sync async c conn userId srcId (Send tgtId msg)

-- TODO: Bulkify this.
expandPointerPostgres :: SyncFunc -> AsyncFunc -> Counter -> Connection -> UserId -> WorkspaceId -> Pointer -> IO ()
expandPointerPostgres sync async c conn userId workspaceId ptr = do
    t <- increment c
    async $ do
        () <$ execute conn "INSERT INTO ExpandedPointers (workspaceId, pointerId, logicalTimeExpanded) VALUES (?, ?, ?) ON CONFLICT DO NOTHING"
                            (workspaceId, ptr, t :: LogicalTime)
    insertCommand sync async c conn userId workspaceId (View ptr)

nextPointerPostgres :: SyncFunc -> AsyncFunc -> Counter -> Connection -> IO Pointer
nextPointerPostgres sync async c conn = do
    sync $ do
        [Only lastPointerId] <- query_ conn "SELECT MAX(id) FROM Pointers"
        return $! maybe 0 succ lastPointerId

createPointersPostgres :: SyncFunc -> AsyncFunc -> Counter -> Connection -> PointerEnvironment -> IO ()
createPointersPostgres sync async c conn env = do
    async $
        () <$ executeMany conn "INSERT INTO Pointers (id, content) VALUES (?, ?)" (M.assocs (fmap (toText . messageToBuilderDB) env))

remapPointersPostgres :: SyncFunc -> AsyncFunc -> Counter -> Connection -> PointerRemapping -> IO ()
remapPointersPostgres sync async c conn mapping = do
    async $
        () <$ executeMany conn "INSERT INTO Pointers (id, content) \
                               \SELECT m.new, p.content \
                               \FROM Pointers p \
                               \INNER JOIN (VALUES (?, ?)) m(new, old) ON m.old = p.id" (M.assocs mapping)

-- NOT CACHEABLE
pendingQuestionsPostgres :: SyncFunc -> AsyncFunc -> Counter -> Connection -> WorkspaceId -> IO [WorkspaceId]
pendingQuestionsPostgres sync async c conn workspaceId = do
    sync $ do
        subquestions <- query conn "SELECT w.id \
                                   \FROM Workspaces w \
                                   \LEFT OUTER JOIN Answers a ON a.workspaceId = w.id \
                                   \WHERE w.parentWorkspaceId = ? \
                                   \  AND a.answer IS NULL ORDER BY logicalTime ASC" (Only workspaceId)
        return $ map (\(Only qId) -> qId) subquestions

-- TODO: Maybe maintain a cache of workspaces.
-- NOT CACHEABLE but the components should be. Cacheable if answered, for now at least.
getWorkspacePostgres :: SyncFunc -> AsyncFunc -> Counter -> Connection -> WorkspaceId -> IO Workspace
getWorkspacePostgres sync async c conn workspaceId = do
    sync $ do
        [(p, t, q)] <- query conn "SELECT parentWorkspaceId, logicalTime, questionAsAnswered \
                                  \FROM Workspaces \
                                  \WHERE id = ? \
                                  \ORDER BY logicalTime DESC LIMIT 1" (Only workspaceId)
        messages <- query conn "SELECT content FROM Messages WHERE targetWorkspaceId = ?" (Only workspaceId) -- TODO: ORDER
        subquestions <- query conn "SELECT w.id, w.questionAsAsked, a.answer \
                                   \FROM Workspaces w \
                                   \LEFT OUTER JOIN Answers a ON w.id = a.workspaceId \
                                   \WHERE w.parentWorkspaceId = ? \
                                   \ORDER BY w.logicalTime ASC" (Only workspaceId)
        expanded <- query conn "SELECT pointerId, content \
                               \FROM ExpandedPointers e \
                               \INNER JOIN Pointers p ON e.pointerId = p.id \
                               \WHERE e.workspaceId = ?" (Only workspaceId)
        return $ Workspace {
            identity = workspaceId,
            parentId = p,
            question = parseMessageUnsafeDB q,
            subQuestions = map (\(qId, q, ma) -> (qId, parseMessageUnsafe q, fmap parseMessageUnsafeDB ma)) subquestions,
            messageHistory = map (\(Only m) -> parseMessageUnsafe m) messages,
            expandedPointers = M.fromList $ map (\(p, m) -> (p, parseMessageUnsafe' p m)) expanded,
            time = Time t }

-- NOT CACHEABLE
getNextWorkspacePostgres :: SyncFunc -> AsyncFunc -> Counter -> Connection -> IO (Maybe WorkspaceId)
getNextWorkspacePostgres sync async c conn = do
    sync $ do
        -- This gets a workspace that doesn't currently have an answer.
        result <- query_ conn "SELECT w.id \
                              \FROM Workspaces w \
                              \WHERE NOT EXISTS(SELECT * FROM Answers a WHERE a.workspaceId = w.id) ORDER BY w.logicalTime DESC LIMIT 1"
        case result of
            [] -> return Nothing
            [Only workspaceId] -> return (Just workspaceId)
