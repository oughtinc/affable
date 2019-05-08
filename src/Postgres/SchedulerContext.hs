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
import Workspace ( Workspace(..), WorkspaceId, VersionId, newWorkspaceId, newVersionId )

makePostgresSchedulerContext :: Queue -> Connection -> IO (SchedulerContext (Connection, Queue))
makePostgresSchedulerContext q conn = do
    [Only t] <- enqueueSync q $ query_ conn "SELECT COUNT(*) FROM WorkspaceVersions"
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
            newVersion = newVersionPostgres sync async c conn,
            createWorkspace = createWorkspacePostgres sync async c conn,
            sendAnswer = sendAnswerPostgres sync async c conn,
            sendMessage = sendMessagePostgres sync async c conn,
            expandPointer = expandPointerPostgres sync async c conn,
            nextPointer = nextPointerPostgres sync async c conn,
            createPointers = createPointersPostgres sync async c conn,
            remapPointers = remapPointersPostgres sync async c conn,
            pendingQuestions = pendingQuestionsPostgres sync async c conn,
            getWorkspace = getWorkspacePostgres sync async c conn,
            workspaceIdOf = workspaceIdOfPostgres sync async c conn,
            getNextWorkspace = getNextWorkspacePostgres sync async c conn,
            dereference = dereferencePostgres sync async c conn,
            reifyWorkspace = reifyWorkspacePostgres sync async c conn,
            extraContent = (conn, q)
        }

-- NOT CACHEABLE
reifyWorkspacePostgres :: SyncFunc -> AsyncFunc -> Counter -> Connection -> VersionId -> IO Message
reifyWorkspacePostgres sync async c conn versionId = do
    workspaces <- sync $ do
        -- TODO: Replace with function.
        execute_ conn "CREATE TEMP TABLE IF NOT EXISTS Descendents ( id UUID PRIMARY KEY, logicalTime INTEGER NOT NULL )"
        execute_ conn "DELETE FROM Descendents"
        execute conn "WITH RECURSIVE ds(id, t) AS ( \
                     \      SELECT v.versionId, v.logicalTime \
                     \      FROM (VALUES (?)) x(id) \
                     \      CROSS JOIN priorVersions(CAST(x.id AS UUID)) y \
                     \      INNER JOIN WorkspaceVersions v ON v.versionId = y.versionId \
                     \ UNION \
                     \      SELECT w.versionId, w.logicalTime FROM WorkspaceVersions w INNER JOIN ds ON w.parentWorkspaceVersionId = ds.id \
                     \) INSERT INTO Descendents ( id, logicalTime ) SELECT id, t FROM ds" (Only versionId)

        workspaces <- query_ conn "SELECT v.versionId, v.workspaceId, v.parentWorkspaceVersionId, v.previousVersion, v.logicalTime, w.questionAsAnswered \
                                  \FROM WorkspaceVersions v \
                                  \INNER JOIN Workspaces w ON w.id = v.workspaceId \
                                  \WHERE v.versionId IN (SELECT id FROM Descendents)"
        messages <- query_ conn "SELECT m.targetWorkspaceVersionId, m.content \
                                \FROM Descendents d \
                                \CROSS JOIN messagesAsOf(d.id) m \
                                \ORDER BY d.logicalTime ASC"
        -- TODO: Ensure ordering is correct.
        subquestions <- query_ conn "SELECT d.id, q.versionId, q.questionAsAsked, q.answer \
                                    \FROM Descendents d \
                                    \CROSS JOIN LATERAL ( \
                                    \   SELECT q.*, RANK() OVER (PARTITION BY q.workspaceId ORDER BY q.logicalTime DESC) AS ranking \
                                    \   FROM subquestionsAsOf(d.id) q) q \
                                    \WHERE q.ranking = 1 \
                                    \ORDER BY d.id ASC, q.logicalTime DESC"
        expanded <- query_ conn "SELECT d.id, e.pointerId, p.content \
                                \FROM Descendents d \
                                \CROSS JOIN expandedPointersAsOf(d.id) e \
                                \INNER JOIN Pointers p ON e.pointerId = p.id"
        let messageMap = M.fromListWith (++) $ map (\(i, m) -> (i, [parseMessageUnsafe m])) messages
            subquestionsMap = M.fromListWith (++) $ map (\(i, qId, q, ma) -> (i, [(qId, parseMessageUnsafe q, fmap parseMessageUnsafeDB ma)])) subquestions
            expandedMap = M.fromListWith M.union $ map (\(i, p, m) -> (i, M.singleton p (parseMessageUnsafe' p m))) expanded
        return $ M.fromList $ map (\(i, wsId, p, pv, t, q) -> (i, Workspace {
                                                                    identity = i,
                                                                    workspaceIdentity = wsId,
                                                                    parentId = p,
                                                                    previousVersion = pv,
                                                                    question = parseMessageUnsafeDB q,
                                                                    subQuestions = maybe [] id $ M.lookup i subquestionsMap,
                                                                    messageHistory = maybe [] id $ M.lookup i messageMap,
                                                                    expandedPointers = maybe M.empty id $ M.lookup i expandedMap,
                                                                    time = Time t })) workspaces
    return (workspaceToMessage workspaces versionId)

-- TODO: Bulkify this.
-- CACHEABLE
dereferencePostgres :: SyncFunc -> AsyncFunc -> Counter -> Connection -> Pointer -> IO Message
dereferencePostgres sync async c conn ptr = do
    sync $ do
        [Only t] <- query conn "SELECT content FROM Pointers WHERE id = ? LIMIT 1" (Only ptr)
        return $! parseMessageUnsafe' ptr t

insertCommand :: SyncFunc -> AsyncFunc -> Counter -> Connection -> UserId -> VersionId -> Command -> IO ()
insertCommand sync async c conn userId versionId cmd = do
    let !cmdText = toText (commandToBuilder cmd)
    async $ do
        mt <- query conn "SELECT commandTime FROM Commands WHERE versionId = ? ORDER BY commandTime DESC LIMIT 1" (Only versionId)
        let t = case mt of [] -> 0; [Only t'] -> t'+1
        () <$ execute conn "INSERT INTO Commands ( versionId, commandTime, userId, command ) VALUES (?, ?, ?, ?)"
                            (versionId, t :: Int64, userId, cmdText)

createInitialWorkspacePostgres :: SyncFunc -> AsyncFunc -> Counter -> Connection -> Message -> IO VersionId
createInitialWorkspacePostgres sync async c conn msg = do
    workspaceId <- newWorkspaceId
    t <- increment c
    let !msgText = toText (messageToBuilder msg)
    vId <- newVersionId
    async $ do
        withTransaction conn $ do
            execute conn "INSERT INTO Workspaces ( id, questionAsAsked, questionAsAnswered ) VALUES (?, ?, ?)"
                                (workspaceId, msgText, msgText)
            () <$ execute conn "INSERT INTO WorkspaceVersions ( versionId, workspaceId, parentWorkspaceVersionId, logicalTime, previousVersion ) \
                               \VALUES (?, ?, ?, ?, ?)"
                                (vId, workspaceId, Nothing :: Maybe VersionId, t, Nothing :: Maybe VersionId)
    insertCommand sync async c conn autoUserId vId (Ask msg)
    return vId

newSessionPostgres :: SyncFunc -> AsyncFunc -> Counter -> Connection -> Maybe SessionId -> IO SessionId
newSessionPostgres sync async c conn Nothing = do
    sessionId <- newSessionId
    newSessionPostgres sync async c conn (Just sessionId)
newSessionPostgres sync async c conn (Just sessionId) = do
    async $ do
        () <$ execute conn "INSERT INTO Sessions ( sessionId ) VALUES (?) ON CONFLICT DO NOTHING" (Only sessionId)
    return sessionId

newVersionPostgres :: SyncFunc -> AsyncFunc -> Counter -> Connection -> VersionId -> IO (VersionId, LogicalTime)
newVersionPostgres sync async c conn versionId = do
    t <- increment c
    vId <- newVersionId
    async $ do
        withTransaction conn $ do
            [(workspaceId, pVId)] <- query conn "SELECT workspaceId, parentWorkspaceVersionId FROM WorkspaceVersions WHERE versionId = ?" (Only versionId)
            () <$ execute conn "INSERT INTO WorkspaceVersions ( versionId, workspaceId, parentWorkspaceVersionId, logicalTime, previousVersion ) \
                               \VALUES (?, ?, ?, ?, ?)"
                                (vId, workspaceId :: WorkspaceId, pVId :: Maybe VersionId, t, Just versionId)
    return (vId, t)

createWorkspacePostgres :: SyncFunc -> AsyncFunc -> Counter -> Connection -> UserId -> VersionId -> Message -> Message -> IO (VersionId, WorkspaceId, LogicalTime)
createWorkspacePostgres sync async c conn userId versionId qAsAsked qAsAnswered = do
    let !qAsAskedText = toText (messageToBuilder qAsAsked)
        !qAsAnsweredText = toText (messageToBuilder qAsAnswered)
    t <- increment c
    childId <- newWorkspaceId
    childVersionId <- newVersionId
    async $ do
        withTransaction conn $ do
            -- TODO: Stick all this into a stored procedure.
            execute conn "INSERT INTO Workspaces ( id, questionAsAsked, questionAsAnswered ) VALUES (?, ?, ?)"
                                (childId, qAsAskedText, qAsAnsweredText)
            () <$ execute conn "INSERT INTO WorkspaceVersions ( versionId, workspaceId, parentWorkspaceVersionId, logicalTime, previousVersion ) \
                               \VALUES (?, ?, ?, ?, ?)"
                                (childVersionId, childId, Just versionId, t, Nothing :: Maybe VersionId)
    insertCommand sync async c conn userId versionId (Ask qAsAsked)
    return (childVersionId, childId, t)

sendAnswerPostgres :: SyncFunc -> AsyncFunc -> Counter -> Connection -> UserId -> VersionId -> Message -> IO ()
sendAnswerPostgres sync async c conn userId versionId msg = do
    let !msgText = toText (messageToBuilder msg)
    async $ do
        () <$ execute conn "INSERT INTO Answers ( versionId, answer ) VALUES (?, ?) \
                           \ON CONFLICT(versionId) DO UPDATE SET answer = excluded.answer"
                            (versionId, msgText)
    insertCommand sync async c conn userId versionId (Reply msg)

sendMessagePostgres :: SyncFunc -> AsyncFunc -> Counter -> Connection -> UserId -> VersionId -> VersionId -> Message -> IO ()
sendMessagePostgres sync async c conn userId srcVersionId tgtVersionId msg = do
    let !msgText = toText (messageToBuilder msg)
    async $ do
        () <$ execute conn "INSERT INTO Messages ( sourceWorkspaceVersionId, targetWorkspaceVersionId, content ) VALUES (?, ?, ?)"
                            (srcVersionId, tgtVersionId, msgText)
    insertCommand sync async c conn userId srcVersionId (Send tgtVersionId msg) -- TODO: Think about this.

-- TODO: Bulkify this.
expandPointerPostgres :: SyncFunc -> AsyncFunc -> Counter -> Connection -> UserId -> VersionId -> Pointer -> IO ()
expandPointerPostgres sync async c conn userId versionId ptr = do
    async $ do
        () <$ execute conn "INSERT INTO ExpandedPointers ( versionId, pointerId ) VALUES (?, ?) ON CONFLICT DO NOTHING" (versionId, ptr)
    insertCommand sync async c conn userId versionId (View ptr)

nextPointerPostgres :: SyncFunc -> AsyncFunc -> Counter -> Connection -> IO Pointer
nextPointerPostgres sync async c conn = do
    sync $ do
        [Only lastPointerId] <- query_ conn "SELECT MAX(id) FROM Pointers"
        return $! maybe 0 succ lastPointerId

createPointersPostgres :: SyncFunc -> AsyncFunc -> Counter -> Connection -> PointerEnvironment -> IO ()
createPointersPostgres sync async c conn env = do
    async $
        () <$ executeMany conn "INSERT INTO Pointers ( id, content ) VALUES (?, ?)" (M.assocs (fmap (toText . messageToBuilderDB) env))

remapPointersPostgres :: SyncFunc -> AsyncFunc -> Counter -> Connection -> PointerRemapping -> IO ()
remapPointersPostgres sync async c conn mapping = do
    async $
        () <$ executeMany conn "INSERT INTO Pointers ( id, content ) \
                               \SELECT m.new, p.content \
                               \FROM Pointers p \
                               \INNER JOIN (VALUES (?, ?)) m(new, old) ON m.old = p.id" (M.assocs mapping)

-- NOT CACHEABLE
pendingQuestionsPostgres :: SyncFunc -> AsyncFunc -> Counter -> Connection -> VersionId -> IO [VersionId]
pendingQuestionsPostgres sync async c conn versionId = do
    sync $ do
        subquestions <- query conn "SELECT w.versionId \
                                   \FROM Current_Subquestions w \
                                   \LEFT OUTER JOIN Answers a ON a.versionId = w.versionId \
                                   \WHERE w.parentWorkspaceVersionId = ? \
                                   \  AND a.answer IS NULL ORDER BY w.logicalTime ASC" (Only versionId)
        return $ map (\(Only qId) -> qId) subquestions

-- TODO: Maybe maintain a cache of workspaces.
-- NOT CACHEABLE but the components should be. Cacheable if answered, for now at least.
getWorkspacePostgres :: SyncFunc -> AsyncFunc -> Counter -> Connection -> VersionId -> IO Workspace
getWorkspacePostgres sync async c conn versionId = do
    sync $ do
        [(wsId, p, pv, t, q)] <- query conn "SELECT v.workspaceId, v.parentWorkspaceVersionId, v.previousVersion, v.logicalTime, w.questionAsAnswered \
                                            \FROM WorkspaceVersions v \
                                            \INNER JOIN Workspaces w ON w.id = v.workspaceId \
                                            \WHERE v.versionId = ?" (Only versionId)
        messages <- query conn "SELECT content FROM Current_Messages WHERE targetWorkspaceVersionId = ?" (Only versionId) -- TODO: ORDER
        subquestions <- query conn "SELECT w.versionId, w.questionAsAsked, w.answer \
                                   \FROM Current_Subquestions w \
                                   \WHERE w.parentWorkspaceVersionId = ? \
                                   \ORDER BY w.logicalTime ASC" (Only versionId)
        expanded <- query conn "SELECT p.id, p.content \
                               \FROM Current_ExpandedPointers e \
                               \INNER JOIN Pointers p ON e.pointerId = p.id \
                               \WHERE e.versionId = ?" (Only versionId)
        return $ Workspace {
            identity = versionId,
            workspaceIdentity = wsId,
            parentId = p,
            previousVersion = pv,
            question = parseMessageUnsafeDB q,
            subQuestions = map (\(qId, q, ma) -> (qId, parseMessageUnsafe q, fmap parseMessageUnsafeDB ma)) subquestions,
            messageHistory = map (\(Only m) -> parseMessageUnsafe m) messages,
            expandedPointers = M.fromList $ map (\(p, m) -> (p, parseMessageUnsafe' p m)) expanded,
            time = Time t }

workspaceIdOfPostgres :: SyncFunc -> AsyncFunc -> Counter -> Connection -> VersionId -> IO WorkspaceId
workspaceIdOfPostgres sync async c conn vId = do
    sync $ do
        [Only workspaceId] <- query conn "SELECT workspaceId FROM WorkspaceVersions WHERE versionId = ? LIMIT 1" (Only vId)
        return workspaceId

-- NOT CACHEABLE
getNextWorkspacePostgres :: SyncFunc -> AsyncFunc -> Counter -> Connection -> IO (Maybe VersionId)
getNextWorkspacePostgres sync async c conn = do
    sync $ do
        -- This gets a workspace that doesn't currently have an answer.
        result <- query_ conn "SELECT w.versionId \
                              \FROM Current_Subquestions w \
                              \WHERE w.answer IS NULL \
                              \ORDER BY w.logicalTime DESC LIMIT 1"
        case result of
            [] -> return Nothing
            [Only workspaceId] -> return (Just workspaceId)
