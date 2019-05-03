{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module Sqlite.SchedulerContext ( makeSqliteSchedulerContext ) where
import Control.Concurrent ( myThreadId ) -- base
import Data.Int ( Int64 ) -- base
import qualified Data.Map as M -- containers
import Data.Text ( Text ) -- text
import Database.SQLite.Simple ( Connection, Only(..), NamedParam(..),
                                query, query_, execute, execute_, executeMany, executeNamed, lastInsertRowId, withTransaction ) -- sqlite-simple

import Command ( Command(..), commandToBuilder )
import Message ( Message(..), Pointer, PointerEnvironment, PointerRemapping,
                 messageToBuilder, messageToBuilderDB, parseMessageUnsafe, parseMessageUnsafe', parseMessageUnsafeDB )
import Scheduler ( SchedulerContext(..), Event, UserId, SessionId, SyncFunc, AsyncFunc,
                   autoUserId, userIdToBuilder, sessionIdToBuilder, newSessionId, workspaceToMessage, eventMessage, renumberEvent )
import Time ( Time(..), LogicalTime )
import Util ( toText, Counter, newCounter, increment, Queue, enqueueAsync, enqueueSync )
import Workspace ( Workspace(..), VersionId, WorkspaceId, newWorkspaceId, workspaceIdToBuilder, workspaceIdFromText,
                   newVersionId, versionIdFromText, versionIdToBuilder )

makeSqliteSchedulerContext :: Queue -> Connection -> IO (SchedulerContext (Connection, Queue))
makeSqliteSchedulerContext q conn = do
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
            createInitialWorkspace = createInitialWorkspaceSqlite sync async c conn,
            newSession = newSessionSqlite sync async c conn,
            newVersion = newVersionSqlite sync async c conn,
            createWorkspace = createWorkspaceSqlite sync async c conn,
            sendAnswer = sendAnswerSqlite sync async c conn,
            sendMessage = sendMessageSqlite sync async c conn,
            expandPointer = expandPointerSqlite sync async c conn,
            nextPointer = nextPointerSqlite sync async c conn,
            createPointers = createPointersSqlite sync async c conn,
            remapPointers = remapPointersSqlite sync async c conn,
            pendingQuestions = pendingQuestionsSqlite sync async c conn,
            getWorkspace = getWorkspaceSqlite sync async c conn,
            workspaceIdOf = workspaceIdOfSqlite sync async c conn,
            getNextWorkspace = getNextWorkspaceSqlite sync async c conn,
            dereference = dereferenceSqlite sync async c conn,
            reifyWorkspace = reifyWorkspaceSqlite sync async c conn,
            extraContent = (conn, q)
        }

-- NOT CACHEABLE
reifyWorkspaceSqlite :: SyncFunc -> AsyncFunc -> Counter -> Connection -> VersionId -> IO Message
reifyWorkspaceSqlite sync async c conn versionId = do
    let !versionIdText = toText (versionIdToBuilder versionId)
    workspaces <- sync $ do
        execute_ conn "CREATE TEMP TABLE IF NOT EXISTS Descendents ( id UUID PRIMARY KEY, workspaceId UUID NOT NULL, logicalTime INTEGER NOT NULL )"
        execute_ conn "DELETE FROM Descendents"
        executeNamed conn "WITH RECURSIVE ds(id, prev, workspaceId, t) AS ( \
                          \     SELECT versionId, previousVersion, workspaceId, logicalTime FROM WorkspaceVersions WHERE versionId = :root \
                          \ UNION \
                          \     SELECT w.versionId, w.previousVersion, w.workspaceId, w.logicalTime \
                          \     FROM WorkspaceVersions w \
                          \     INNER JOIN ds ON w.parentWorkspaceVersionId = ds.id OR ds.prev = w.versionId \
                          \) INSERT INTO Descendents ( id, workspaceId, logicalTime ) \
                          \  SELECT id, workspaceId, t FROM ds" [":root" := versionIdText]
        workspaces <- query_ conn "SELECT v.versionId, v.workspaceId, v.parentWorkspaceVersionId, v.previousVersion, v.logicalTime, w.questionAsAnswered \
                                  \FROM Descendents d \
                                  \INNER JOIN WorkspaceVersions v ON v.versionId = d.id \
                                  \INNER JOIN Workspaces w ON w.id = v.workspaceId"
        messages <- query_ conn "SELECT d.id, m.content \
                                \FROM Descendents d \
                                \INNER JOIN WorkspaceVersions tgt ON tgt.workspaceId = d.workspaceId AND tgt.logicalTime <= d.logicalTime \
                                \INNER JOIN Messages m ON m.targetWorkspaceVersionId = tgt.versionId \
                                \WHERE tgt.versionId IN (SELECT id FROM Descendents) \
                                \ORDER BY tgt.logicalTime ASC"
        subquestions <- query_ conn "SELECT d.id, q.versionId, qw.questionAsAsked, a.answer \
                                    \FROM Descendents d \
                                    \INNER JOIN WorkspaceVersions p ON p.workspaceId = d.workspaceId AND p.logicalTime <= d.logicalTime \
                                    \INNER JOIN WorkspaceVersions q ON q.parentWorkspaceVersionId = p.versionId \
                                    \INNER JOIN Workspaces qw ON qw.id = q.workspaceId \
                                    \LEFT OUTER JOIN Answers a ON q.versionId = a.versionId \
                                    \WHERE p.versionId IN (SELECT id FROM Descendents) \
                                    \ORDER BY p.versionId ASC, q.logicalTime DESC"
        {-
        expanded <- query_ conn "SELECT versionId, pointerId, content \
                                \FROM ExpandedPointers e \
                                \INNER JOIN Pointers p ON e.pointerId = p.id \
                                \WHERE versionId IN (SELECT id FROM Descendents)"
        -}
        let messageMap = M.fromListWith (++) $ map (\(i, m) -> (versionIdFromText i, [parseMessageUnsafe m])) messages
            subquestionsMap = M.fromListWith (++) $
                                map (\(i, qId, q, ma) ->
                                        (versionIdFromText i, [(versionIdFromText qId, parseMessageUnsafe q, fmap parseMessageUnsafeDB ma)])) subquestions
            expandedMap = M.empty -- M.fromListWith M.union $ map (\(i, p, m) -> (i, M.singleton p (parseMessageUnsafe' p m))) expanded
        return $ M.fromList $ map (\(i, wsId, p, pv, t, q) -> let !vId = versionIdFromText i
                                                              in (vId, Workspace {
                                                                        identity = vId,
                                                                        workspaceIdentity = workspaceIdFromText wsId,
                                                                        parentId = versionIdFromText <$> p,
                                                                        previousVersion = versionIdFromText <$> pv,
                                                                        question = parseMessageUnsafeDB q,
                                                                        subQuestions = maybe [] id $ M.lookup vId subquestionsMap,
                                                                        messageHistory = maybe [] id $ M.lookup vId messageMap,
                                                                        expandedPointers = maybe M.empty id $ M.lookup vId expandedMap,
                                                                        time = Time t })) workspaces
    return (workspaceToMessage workspaces versionId)

-- TODO: Bulkify this.
-- CACHEABLE
dereferenceSqlite :: SyncFunc -> AsyncFunc -> Counter -> Connection -> Pointer -> IO Message
dereferenceSqlite sync async c conn ptr = do
    sync $ do
        [Only t] <- query conn "SELECT content FROM Pointers WHERE id = ? LIMIT 1" (Only ptr)
        return $! parseMessageUnsafe' ptr t

insertCommand :: SyncFunc -> AsyncFunc -> Counter -> Connection -> UserId -> VersionId -> Command -> IO ()
insertCommand sync async c conn userId versionId cmd = do
    let !cmdText = toText (commandToBuilder cmd)
    let !versionIdText = toText (versionIdToBuilder versionId)
    async $ do
        mt <- query conn "SELECT commandTime FROM Commands WHERE versionId = ? ORDER BY commandTime DESC LIMIT 1" (Only versionIdText)
        let t = case mt of [] -> 0; [Only t'] -> t'+1
        executeNamed conn "INSERT INTO Commands (versionId, commandTime, userId, command) VALUES (:versionId, :time, :userId, :cmd)" [
                            ":versionId" := versionIdText,
                            ":time" := (t :: Int64),
                            ":userId" := toText (userIdToBuilder userId),
                            ":cmd" := cmdText]

createInitialWorkspaceSqlite :: SyncFunc -> AsyncFunc -> Counter -> Connection -> Message -> IO VersionId
createInitialWorkspaceSqlite sync async c conn msg = do
    workspaceId <- newWorkspaceId
    let !wsIdText = toText (workspaceIdToBuilder workspaceId)
    let !msgText = toText (messageToBuilder msg)
    t <- increment c
    versionId <- newVersionId
    let !vIdText = toText (versionIdToBuilder versionId)
    async $ do
        withTransaction conn $ do
            executeNamed conn "INSERT INTO Workspaces (id, questionAsAsked, questionAsAnswered) \
                              \VALUES (:id, :questionAsAsked, :questionAsAnswered)" [
                                ":id" := wsIdText,
                                ":questionAsAsked" := msgText,
                                ":questionAsAnswered" := msgText]
            executeNamed conn "INSERT INTO WorkspaceVersions ( versionId, workspaceId, parentWorkspaceVersionId, logicalTime, previousVersion ) \
                              \VALUES (:versionId, :workspaceId, :parent, :time, :prevVersion)" [
                                ":versionId" := vIdText,
                                ":workspaceId" := wsIdText,
                                ":parent" := (Nothing :: Maybe Text),
                                ":time" := t,
                                ":prevVersion" := (Nothing :: Maybe Text)]
    insertCommand sync async c conn autoUserId versionId (Ask msg)
    return versionId

newSessionSqlite :: SyncFunc -> AsyncFunc -> Counter -> Connection -> Maybe SessionId -> IO SessionId
newSessionSqlite sync async c conn Nothing = do
    sessionId <- newSessionId
    newSessionSqlite sync async c conn (Just sessionId)
newSessionSqlite sync async c conn (Just sessionId) = do
    async $
        execute conn "INSERT OR IGNORE INTO Sessions ( sessionId ) VALUES (?)" (Only (toText (sessionIdToBuilder sessionId)))
    return sessionId

newVersionSqlite :: SyncFunc -> AsyncFunc -> Counter -> Connection -> VersionId -> IO (VersionId, LogicalTime)
newVersionSqlite sync async c conn versionId = do
    let !versionIdText = toText (versionIdToBuilder versionId)
    vId <- newVersionId
    t <- increment c
    let !vIdText = toText (versionIdToBuilder vId)
    async $ do
        withTransaction conn $ do
            [(workspaceIdText, pVIdText)] <- query conn "SELECT workspaceId, parentWorkspaceVersionId FROM WorkspaceVersions WHERE versionId = ?"
                                            (Only versionIdText)
            executeNamed conn "INSERT INTO WorkspaceVersions ( versionId, workspaceId, parentWorkspaceVersionId, logicalTime, previousVersion ) \
                              \VALUES (:versionId, :workspaceId, :parent, :time, :prevVersion)" [
                                ":versionId" := vIdText,
                                ":workspaceId" := (workspaceIdText :: Text),
                                ":parent" := (pVIdText :: Maybe Text),
                                ":time" := t,
                                ":prevVersion" := Just versionIdText]
    return (vId, t)

createWorkspaceSqlite :: SyncFunc -> AsyncFunc -> Counter -> Connection -> UserId -> VersionId -> Message -> Message -> IO (VersionId, WorkspaceId, LogicalTime)
createWorkspaceSqlite sync async c conn userId versionId qAsAsked qAsAnswered = do
    let !qAsAskedText = toText (messageToBuilder qAsAsked)
        !qAsAnsweredText = toText (messageToBuilder qAsAnswered)
    childId <- newWorkspaceId
    let !versionIdText = toText (versionIdToBuilder versionId)
    let !childIdText = toText (workspaceIdToBuilder childId)
    t <- increment c
    childVersionId <- newVersionId
    let !childVersionIdText = toText (versionIdToBuilder childVersionId)
    async $ do
        withTransaction conn $ do
            executeNamed conn "INSERT INTO Workspaces ( id, questionAsAsked, questionAsAnswered ) \
                              \VALUES (:id, :asAsked, :asAnswered)" [
                                ":id" := childIdText,
                                ":asAsked" := qAsAskedText,
                                ":asAnswered" := qAsAnsweredText]
            executeNamed conn "INSERT INTO WorkspaceVersions ( versionId, workspaceId, parentWorkspaceVersionId, logicalTime, previousVersion ) \
                              \VALUES (:versionId, :workspaceId, :parent, :time, :prevVersion)" [
                                ":versionId" := childVersionIdText,
                                ":workspaceId" := childIdText,
                                ":parent" := Just versionIdText,
                                ":time" := t,
                                ":prevVersion" := (Nothing :: Maybe Text)]
    insertCommand sync async c conn userId versionId (Ask qAsAsked)
    return (childVersionId, childId, t)

sendAnswerSqlite :: SyncFunc -> AsyncFunc -> Counter -> Connection -> UserId -> VersionId -> Message -> IO ()
sendAnswerSqlite sync async c conn userId versionId msg = do
    let !msgText = toText (messageToBuilder msg)
    let !versionIdText = toText (versionIdToBuilder versionId)
    async $ do
        executeNamed conn "INSERT INTO Answers (versionId, answer) VALUES (:versionId, :answer)" [
                            ":versionId" := versionIdText,
                            ":answer" := msgText]
    insertCommand sync async c conn userId versionId (Reply msg)

sendMessageSqlite :: SyncFunc -> AsyncFunc -> Counter -> Connection -> UserId -> VersionId -> VersionId -> Message -> IO ()
sendMessageSqlite sync async c conn userId srcVersionId tgtVersionId msg = do
    let !msgText = toText (messageToBuilder msg)
    let !srcVersionIdText = toText (versionIdToBuilder srcVersionId)
    let !tgtVersionIdText = toText (versionIdToBuilder tgtVersionId)
    async $ do
        executeNamed conn "INSERT INTO Messages (sourceWorkspaceVersionId, targetWorkspaceVersionId, content) \
                          \VALUES (:source, :target, :content)" [
                            ":source" := srcVersionIdText,
                            ":target" := tgtVersionIdText,
                            ":content" := msgText]
    insertCommand sync async c conn userId srcVersionId (Send tgtVersionId msg)

-- TODO: Bulkify this.
expandPointerSqlite :: SyncFunc -> AsyncFunc -> Counter -> Connection -> UserId -> VersionId -> Pointer -> IO ()
expandPointerSqlite sync async c conn userId versionId ptr = do
    let !versionIdText = toText (versionIdToBuilder versionId)
    async $ do
        executeNamed conn "INSERT OR IGNORE INTO ExpandedPointers (versionId, pointerId) VALUES (:versionId, :pointer)" [
                            ":versionId" := versionIdText,
                            ":pointer" := ptr]
    insertCommand sync async c conn userId versionId (View ptr)

nextPointerSqlite :: SyncFunc -> AsyncFunc -> Counter -> Connection -> IO Pointer
nextPointerSqlite sync async c conn = do
    sync $ do
        [Only lastPointerId] <- query_ conn "SELECT MAX(id) FROM Pointers"
        return $! maybe 0 succ lastPointerId

createPointersSqlite :: SyncFunc -> AsyncFunc -> Counter -> Connection -> PointerEnvironment -> IO ()
createPointersSqlite sync async c conn env = do
    async $ do
        executeMany conn "INSERT INTO Pointers (id, content) VALUES (?, ?)" (M.assocs (fmap (toText . messageToBuilderDB) env))

remapPointersSqlite :: SyncFunc -> AsyncFunc -> Counter -> Connection -> PointerRemapping -> IO ()
remapPointersSqlite sync async c conn mapping = do
    async $ do
        executeMany conn "INSERT INTO Pointers (id, content) \
                         \SELECT ?, content \
                         \FROM Pointers \
                         \WHERE id = ?" (M.assocs mapping)

-- NOT CACHEABLE
pendingQuestionsSqlite :: SyncFunc -> AsyncFunc -> Counter -> Connection -> VersionId -> IO [VersionId]
pendingQuestionsSqlite sync async c conn versionId = do
    let !versionIdText = toText (versionIdToBuilder versionId)
    sync $ do
        subquestions <- query conn "SELECT w.versionId \
                                   \FROM Current_Subquestions w \
                                   \LEFT OUTER JOIN Answers a ON a.versionId = w.versionId \
                                   \WHERE w.parentWorkspaceVersionId = ? \
                                   \  AND a.answer IS NULL ORDER BY w.logicalTime ASC" (Only versionIdText)
        return $ map (\(Only qId) -> versionIdFromText qId) subquestions

-- TODO: Maybe maintain a cache of workspaces.
-- NOT CACHEABLE but the components should be. Cacheable if answered, for now at least.

getWorkspaceSqlite :: SyncFunc -> AsyncFunc -> Counter -> Connection -> VersionId -> IO Workspace
getWorkspaceSqlite sync async c conn versionId = do
    let !versionIdText = toText (versionIdToBuilder versionId)
    sync $ do
        [(wsId, p, pv, t, q)] <- query conn "SELECT v.workspaceId, v.parentWorkspaceVersionId, v.previousVersion, v.logicalTime, w.questionAsAnswered \
                                            \FROM WorkspaceVersions v \
                                            \INNER JOIN Workspaces w ON w.id = v.workspaceId \
                                            \WHERE v.versionId = ?" (Only versionIdText)
        messages <- query conn "SELECT content FROM Current_Messages WHERE targetWorkspaceVersionId = ?" (Only versionIdText) -- TODO: ORDER
        subquestions <- query conn "SELECT w.versionId, w.questionAsAsked, w.answer \
                                   \FROM Current_Subquestions w \
                                   \WHERE w.parentWorkspaceVersionId = ? \
                                   \ORDER BY w.logicalTime ASC" (Only versionIdText)
        expanded <- query conn "SELECT p.id, p.content \
                               \FROM Current_ExpandedPointers e \
                               \INNER JOIN Pointers p ON e.pointerId = p.id \
                               \WHERE e.versionId = ?" (Only versionIdText)
        return $ Workspace {
            identity = versionId,
            workspaceIdentity = workspaceIdFromText wsId,
            parentId = versionIdFromText <$> p,
            previousVersion = versionIdFromText <$> pv,
            question = parseMessageUnsafeDB q,
            subQuestions = map (\(qId, q, ma) -> (versionIdFromText qId, parseMessageUnsafe q, fmap parseMessageUnsafeDB ma)) subquestions,
            messageHistory = map (\(Only m) -> parseMessageUnsafe m) messages,
            expandedPointers = M.fromList $ map (\(p, m) -> (p, parseMessageUnsafe' p m)) expanded,
            time = Time t }

workspaceIdOfSqlite :: SyncFunc -> AsyncFunc -> Counter -> Connection -> VersionId -> IO WorkspaceId
workspaceIdOfSqlite sync async c conn vId = do
    let !vIdText = toText (versionIdToBuilder vId)
    sync $ do
        [Only workspaceId] <- query conn "SELECT workspaceId FROM WorkspaceVersions WHERE versionId = ? LIMIT 1" (Only vIdText)
        return $ workspaceIdFromText workspaceId

-- NOT CACHEABLE
getNextWorkspaceSqlite :: SyncFunc -> AsyncFunc -> Counter -> Connection -> IO (Maybe VersionId)
getNextWorkspaceSqlite sync async c conn = do
    sync $ do
        -- This gets a workspace that doesn't currently have an answer.
        result <- query_ conn "SELECT w.versionId \
                              \FROM Current_Subquestions w \
                              \WHERE w.answer IS NULL \
                              \ORDER BY w.logicalTime DESC LIMIT 1"
        case result of
            [] -> return Nothing
            [Only versionId] -> return (Just $ versionIdFromText versionId)
