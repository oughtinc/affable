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
import Workspace ( Workspace(..), VersionId, newWorkspaceId, workspaceIdToBuilder, newVersionId, versionIdFromText, versionIdToBuilder )

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
            createWorkspace = createWorkspaceSqlite sync async c conn,
            sendAnswer = sendAnswerSqlite sync async c conn,
            sendMessage = sendMessageSqlite sync async c conn,
            expandPointer = expandPointerSqlite sync async c conn,
            nextPointer = nextPointerSqlite sync async c conn,
            createPointers = createPointersSqlite sync async c conn,
            remapPointers = remapPointersSqlite sync async c conn,
            pendingQuestions = pendingQuestionsSqlite sync async c conn,
            getWorkspace = getWorkspaceSqlite sync async c conn,
            getNextWorkspace = getNextWorkspaceSqlite sync async c conn,
            dereference = dereferenceSqlite sync async c conn,
            reifyWorkspace = reifyWorkspaceSqlite sync async c conn,
            extraContent = (conn, q)
        }

-- TODO: XXX This needs to be reworked significantly to incorporate versioning.
-- NOT CACHEABLE
reifyWorkspaceSqlite :: SyncFunc -> AsyncFunc -> Counter -> Connection -> VersionId -> IO Message
reifyWorkspaceSqlite sync async c conn versionId = do
    let !versionIdText = toText (versionIdToBuilder versionId)
    workspaces <- sync $ do
        execute_ conn "CREATE TEMP TABLE IF NOT EXISTS Descendants ( id INTEGER PRIMARY KEY ASC )"
        execute_ conn "DELETE FROM Descendants"
        executeNamed conn "WITH RECURSIVE ds(id) AS ( \
                          \     VALUES (:root) \
                          \ UNION ALL \
                          \     SELECT w.id FROM Workspaces w INNER JOIN ds ON w.parentWorkspaceVersionId = ds.id \
                          \) INSERT INTO Descendants ( id ) SELECT id FROM ds" [":root" := versionIdText]
        workspaces <- query_ conn "SELECT id, parentWorkspaceVersionId, logicalTime, questionAsAnswered \
                                  \FROM Workspaces WHERE id IN (SELECT id FROM Descendants)"
        messages <- query_ conn "SELECT targetWorkspaceVersionId, content \
                                \FROM Messages \
                                \WHERE targetWorkspaceVersionId IN (SELECT id FROM Descendants) \
                                \ORDER BY id ASC"
        subquestions <- query_ conn "SELECT p.id, q.id, q.questionAsAsked, a.answer \
                                    \FROM Workspaces p \
                                    \INNER JOIN Workspaces q ON q.parentWorkspaceVersionId = p.id \
                                    \LEFT OUTER JOIN Answers a ON q.id = a.versionId \
                                    \WHERE p.id IN (SELECT id FROM Descendants) \
                                    \ORDER BY p.id ASC, q.logicalTime DESC"
        {-
        expanded <- query_ conn "SELECT versionId, pointerId, content \
                                \FROM ExpandedPointers e \
                                \INNER JOIN Pointers p ON e.pointerId = p.id \
                                \WHERE versionId IN (SELECT id FROM Descendants)"
        -}
        let messageMap = M.fromListWith (++) $ map (\(i, m) -> (versionIdFromText i, [parseMessageUnsafe m])) messages
            subquestionsMap = M.fromListWith (++) $
                                map (\(i, qId, q, ma) ->
                                        (versionIdFromText i, [(versionIdFromText qId, parseMessageUnsafe q, fmap parseMessageUnsafeDB ma)])) subquestions
            expandedMap = M.empty -- M.fromListWith M.union $ map (\(i, p, m) -> (i, M.singleton p (parseMessageUnsafe' p m))) expanded
        return $ M.fromList $ map (\(i, p, t, q) -> let !wsId = versionIdFromText i
                                                    in (wsId, Workspace {
                                                                identity = wsId,
                                                                parentId = versionIdFromText <$> p,
                                                                question = parseMessageUnsafeDB q,
                                                                subQuestions = maybe [] id $ M.lookup wsId subquestionsMap,
                                                                messageHistory = maybe [] id $ M.lookup wsId messageMap,
                                                                expandedPointers = maybe M.empty id $ M.lookup wsId expandedMap,
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
            executeNamed conn "INSERT INTO Workspaces (id, parentWorkspaceVersionId, questionAsAsked, questionAsAnswered) \
                              \VALUES (:id, :parent, :questionAsAsked, :questionAsAnswered)" [
                                ":id" := wsIdText,
                                ":parent" := (Nothing :: Maybe Text),
                                ":questionAsAsked" := msgText,
                                ":questionAsAnswered" := msgText]
            executeNamed conn "INSERT INTO WorkspaceVersions ( versionId, workspaceId, logicalTime, previousVersion ) \
                              \VALUES (:versionId, :workspaceId, :time, :prevVersion)" [
                                ":versionId" := vIdText,
                                ":workspaceId" := wsIdText,
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

createWorkspaceSqlite :: SyncFunc -> AsyncFunc -> Counter -> Connection -> UserId -> VersionId -> Message -> Message -> IO (VersionId, VersionId)
createWorkspaceSqlite sync async c conn userId versionId qAsAsked qAsAnswered = do
    let !qAsAskedText = toText (messageToBuilder qAsAsked)
        !qAsAnsweredText = toText (messageToBuilder qAsAnswered)
    childId <- newWorkspaceId
    let !versionIdText = toText (versionIdToBuilder versionId)
    let !childIdText = toText (workspaceIdToBuilder childId)
    t <- increment c
    vId <- newVersionId
    let !vIdText = toText (versionIdToBuilder vId)
    childVersionId <- newVersionId
    let !childVersionIdText = toText (versionIdToBuilder childVersionId)
    async $ do
        withTransaction conn $ do
            [Only workspaceId] <- query conn "SELECT w.workspaceId FROM WorkspaceVersions w WHERE w.versionId = ?" (Only versionIdText)
            executeNamed conn "INSERT INTO WorkspaceVersions ( versionId, workspaceId, logicalTime, previousVersion ) \
                               \VALUES (:versionId, :workspaceId, :time, :prevVersion)" [
                                ":versionId" := vIdText,
                                ":workspaceId" := (workspaceId :: Text),
                                ":time" := t,
                                ":prevVersion" := Just versionIdText]
            executeNamed conn "INSERT INTO Workspaces ( id, parentWorkspaceVersionId, questionAsAsked, questionAsAnswered ) \
                              \VALUES (:id, :parent, :asAsked, :asAnswered)" [
                                ":id" := childIdText,
                                ":parent" := Just vIdText,
                                ":asAsked" := qAsAskedText,
                                ":asAnswered" := qAsAnsweredText]
            executeNamed conn "INSERT INTO WorkspaceVersions ( versionId, workspaceId, logicalTime, previousVersion ) \
                              \VALUES (:versionId, :workspaceId, :time, :prevVersion)" [
                                ":versionId" := childVersionIdText,
                                ":workspaceId" := childIdText,
                                ":time" := t,
                                ":prevVersion" := (Nothing :: Maybe Text)]
    insertCommand sync async c conn userId vId (Ask qAsAsked)
    return (vId, childVersionId)

sendAnswerSqlite :: SyncFunc -> AsyncFunc -> Counter -> Connection -> UserId -> VersionId -> Message -> IO VersionId
sendAnswerSqlite sync async c conn userId versionId msg = do
    let !msgText = toText (messageToBuilder msg)
    let !versionIdText = toText (versionIdToBuilder versionId)
    vId <- newVersionId
    let !vIdText = toText (versionIdToBuilder vId)
    t <- increment c
    async $ do
        withTransaction conn $ do
            [Only workspaceId] <- query conn "SELECT w.workspaceId FROM WorkspaceVersions w WHERE w.versionId = ?" (Only versionIdText)
            executeNamed conn "INSERT INTO WorkspaceVersions ( versionId, workspaceId, logicalTime, previousVersion ) \
                               \VALUES (:versionId, :workspaceId, :time, :prevVersion)" [
                                ":versionId" := vIdText,
                                ":workspaceId" := (workspaceId :: Text),
                                ":time" := t,
                                ":prevVersion" := Just versionIdText]
            -- TODO: XXX If we revisit, and thus change an answer, this will need to be an INSERT OR REPLACE or we'll need to start
            -- actually using this time parameter. If this is all that is changed, then we'll get a model of edits where we see
            -- the following questions upon return, possibly referring to pointers in an answer that no longer exist.
            executeNamed conn "INSERT OR REPLACE INTO Answers (versionId, answer) VALUES (:versionId, :answer)" [
                                ":versionId" := vIdText,
                                ":answer" := msgText]
    insertCommand sync async c conn userId vId (Reply msg)
    return vId

sendMessageSqlite :: SyncFunc -> AsyncFunc -> Counter -> Connection -> UserId -> VersionId -> VersionId -> Message -> IO VersionId
sendMessageSqlite sync async c conn userId srcVersionId tgtVersionId msg = do
    let !msgText = toText (messageToBuilder msg)
    let !srcVersionIdText = toText (versionIdToBuilder srcVersionId)
    let !tgtVersionIdText = toText (versionIdToBuilder tgtVersionId)
    vId <- newVersionId
    let !vIdText = toText (versionIdToBuilder vId)
    t <- increment c
    async $ do
        withTransaction conn $ do
            [Only tgtId] <- query conn "SELECT w.workspaceId FROM WorkspaceVersions w WHERE w.versionId = ?" (Only tgtVersionIdText)
            executeNamed conn "INSERT INTO WorkspaceVersions ( versionId, workspaceId, logicalTime, previousVersion ) \
                               \VALUES (:versionId, :workspaceId, :time, :prevVersion)" [
                                ":versionId" := vIdText,
                                ":workspaceId" := (tgtId :: Text),
                                ":time" := t,
                                ":prevVersion" := Just tgtVersionIdText]
            executeNamed conn "INSERT INTO Messages (sourceWorkspaceVersionId, targetWorkspaceVersionId, content) \
                              \VALUES (:source, :target, :content)" [
                                ":source" := srcVersionIdText,
                                ":target" := vIdText,
                                ":content" := msgText]
    insertCommand sync async c conn userId srcVersionId (Send vId msg)
    return vId

-- TODO: Bulkify this.
expandPointerSqlite :: SyncFunc -> AsyncFunc -> Counter -> Connection -> UserId -> VersionId -> Pointer -> IO VersionId
expandPointerSqlite sync async c conn userId versionId ptr = do
    let !versionIdText = toText (versionIdToBuilder versionId)
    t <- increment c
    vId <- newVersionId
    let !vIdText = toText (versionIdToBuilder vId)
    async $ do
        withTransaction conn $ do
            [Only workspaceId] <- query conn "SELECT w.workspaceId FROM WorkspaceVersions w WHERE w.versionId = ?" (Only versionIdText)
            executeNamed conn "INSERT INTO WorkspaceVersions ( versionId, workspaceId, logicalTime, previousVersion ) \
                               \VALUES (:versionId, :workspaceId, :time, :prevVersion)" [
                                ":versionId" := vIdText,
                                ":workspaceId" := (workspaceId :: Text),
                                ":time" := t,
                                ":prevVersion" := Just versionIdText]
            executeNamed conn "INSERT OR IGNORE INTO ExpandedPointers (versionId, pointerId) VALUES (:versionId, :pointer)" [
                                ":versionId" := vIdText,
                                ":pointer" := ptr]
    insertCommand sync async c conn userId vId (View ptr)
    return vId

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
        [(p, t, q)] <- query conn "SELECT w.parentWorkspaceVersionId, v.logicalTime, w.questionAsAnswered \
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
            parentId = versionIdFromText <$> p,
            question = parseMessageUnsafeDB q,
            subQuestions = map (\(qId, q, ma) -> (versionIdFromText qId, parseMessageUnsafe q, fmap parseMessageUnsafeDB ma)) subquestions,
            messageHistory = map (\(Only m) -> parseMessageUnsafe m) messages,
            expandedPointers = M.fromList $ map (\(p, m) -> (p, parseMessageUnsafe' p m)) expanded,
            time = Time t }

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
