{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module Postgres.SchedulerContext ( makePostgresSchedulerContext ) where
import Control.Exception ( bracket_ ) -- base
import Data.Int ( Int64 ) -- base
import Data.IORef ( IORef, newIORef, writeIORef, readIORef ) -- base
import qualified Data.Map as M -- containers
import Data.Text ( Text ) -- text
import Database.PostgreSQL.Simple ( Connection, Only(..),
                                    query, query_, execute, execute_, executeMany, withTransaction ) -- postgresql-simple

import Command ( Command(..), commandToBuilder )
import Message ( Message(..), Pointer, PointerEnvironment, PointerRemapping,
                 messageToBuilder, messageToBuilderDB, parseMessageUnsafe, parseMessageUnsafe', parseMessageUnsafeDB )
import Scheduler ( SchedulerContext(..), Event, UserId, SessionId, SyncFunc,
                   autoUserId, workspaceToMessage, eventMessage, renumberEvent, newSessionId )
import Time ( Time(..), LogicalTime )
import Util ( toText, Counter, newCounter, increment, Queue, enqueueAsync, enqueueSync )
import Workspace ( Workspace(..), WorkspaceId, newWorkspaceId )

makePostgresSchedulerContext :: Queue -> Connection -> IO (SchedulerContext (Connection, Queue))
makePostgresSchedulerContext q conn = do
    [Only t] <- enqueueSync q $ query_ conn "SELECT COUNT(*) FROM Commands"
    c <- newCounter t
    nestedRef <- newIORef False
    let sync action = do
            nested <- readIORef nestedRef
            if nested then action else enqueueSync q (bracket_ (writeIORef nestedRef True) (writeIORef nestedRef False) action)
    return $
        SchedulerContext {
            doAtomically = sync,
            createInitialWorkspace = createInitialWorkspacePostgres sync q c conn,
            newSession = newSessionPostgres sync q c conn,
            createWorkspace = createWorkspacePostgres sync q c conn,
            sendAnswer = sendAnswerPostgres sync q c conn,
            sendMessage = sendMessagePostgres sync q c conn,
            expandPointer = expandPointerPostgres sync q c conn,
            nextPointer = nextPointerPostgres sync q c conn,
            createPointers = createPointersPostgres sync q c conn,
            remapPointers = remapPointersPostgres sync q c conn,
            pendingQuestions = pendingQuestionsPostgres sync q c conn,
            getWorkspace = getWorkspacePostgres sync q c conn,
            allWorkspaces = allWorkspacesPostgres sync q c conn,
            getNextWorkspace = getNextWorkspacePostgres sync q c conn,
            dereference = dereferencePostgres sync q c conn,
            reifyWorkspace = reifyWorkspacePostgres sync q c conn,
            extraContent = (conn, q)
        }

-- NOT CACHEABLE
reifyWorkspacePostgres :: SyncFunc -> Queue -> Counter -> Connection -> WorkspaceId -> IO Message
reifyWorkspacePostgres sync q c conn workspaceId = do
    workspaces <- sync $ do
        withTransaction conn $ do
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
dereferencePostgres :: SyncFunc -> Queue -> Counter -> Connection -> Pointer -> IO Message
dereferencePostgres sync q c conn ptr = do
    sync $ do
        [Only t] <- query conn "SELECT content FROM Pointers WHERE id = ? LIMIT 1" (Only ptr)
        return $! parseMessageUnsafe' ptr t

insertCommand :: SyncFunc -> Queue -> Counter -> Connection -> UserId -> WorkspaceId -> Command -> IO ()
insertCommand sync q c conn userId workspaceId cmd = do
    let !cmdText = toText (commandToBuilder cmd)
    enqueueAsync q $ do
        mt <- query conn "SELECT commandTime FROM Commands WHERE workspaceId = ? ORDER BY commandTime DESC LIMIT 1" (Only workspaceId)
        let t = case mt of [] -> 0; [Only t'] -> t'+1
        () <$ execute conn "INSERT INTO Commands (workspaceId, commandTime, userId, command) VALUES (?, ?, ?, ?)"
                            (workspaceId, t :: Int64, userId, cmdText)

createInitialWorkspacePostgres :: SyncFunc -> Queue -> Counter -> Connection -> IO WorkspaceId
createInitialWorkspacePostgres sync q c conn = do
    workspaceId <- newWorkspaceId
    t <- increment c
    let msg = Text "What is your question?"
    enqueueAsync q $ do
        withTransaction conn $ do
            [Only lastPointerId] <- query_ conn "SELECT MAX(id) FROM Pointers"
            let !p = maybe 0 succ lastPointerId
            let msg' = LabeledStructured p [msg]
            let !msgText = toText (messageToBuilder msg)
                !msgText' = toText (messageToBuilder msg')
            () <$ execute conn "INSERT INTO Workspaces (id, logicalTime, parentWorkspaceId, questionAsAsked, questionAsAnswered) \
                               \VALUES (?, ?, ?, ?, ?)"
                                 (workspaceId, t :: LogicalTime, Nothing :: Maybe WorkspaceId, msgText, msgText')
    insertCommand sync q c conn autoUserId workspaceId (Ask msg)
    return workspaceId

newSessionPostgres :: SyncFunc -> Queue -> Counter -> Connection -> Maybe SessionId -> IO SessionId
newSessionPostgres sync q c conn Nothing = do
    sessionId <- newSessionId
    newSessionPostgres sync q c conn (Just sessionId)
newSessionPostgres sync q c conn (Just sessionId) = do
    enqueueAsync q $ do
        () <$ execute conn "INSERT INTO Sessions (sessionId) VALUES (?) ON CONFLICT DO NOTHING" (Only sessionId)
    return sessionId

createWorkspacePostgres :: SyncFunc -> Queue -> Counter -> Connection -> UserId -> WorkspaceId -> Message -> Message -> IO WorkspaceId
createWorkspacePostgres sync q c conn userId workspaceId qAsAsked qAsAnswered = do
    let !qAsAskedText = toText (messageToBuilder qAsAsked)
        !qAsAnsweredText = toText (messageToBuilder qAsAnswered)
    wsId <- newWorkspaceId
    t <- increment c
    enqueueAsync q $ do
        () <$ execute conn "INSERT INTO Workspaces (id, logicalTime, parentWorkspaceId, questionAsAsked, questionAsAnswered) \
                           \VALUES (?, ?, ?, ?, ?)"
                            (wsId, t :: LogicalTime, Just workspaceId, qAsAskedText, qAsAnsweredText)
    insertCommand sync q c conn userId workspaceId (Ask qAsAsked)
    return wsId

sendAnswerPostgres :: SyncFunc -> Queue -> Counter -> Connection -> UserId -> WorkspaceId -> Message -> IO ()
sendAnswerPostgres sync q c conn userId workspaceId msg = do
    let !msgText = toText (messageToBuilder msg)
    t <- increment c
    enqueueAsync q $ do
        () <$ execute conn "INSERT INTO Answers (workspaceId, logicalTimeAnswered, answer) VALUES (?, ?, ?) \
                           \ON CONFLICT(workspaceId) DO UPDATE SET logicalTimeAnswered = excluded.logicalTimeAnswered, answer = excluded.answer"
                            (workspaceId, t :: LogicalTime, msgText)
    insertCommand sync q c conn userId workspaceId (Reply msg)

sendMessagePostgres :: SyncFunc -> Queue -> Counter -> Connection -> UserId -> WorkspaceId -> WorkspaceId -> Message -> IO ()
sendMessagePostgres sync q c conn userId srcId tgtId msg = do
    let !msgText = toText (messageToBuilder msg)
    t <- increment c
    enqueueAsync q $ do
        () <$ execute conn "INSERT INTO Messages (sourceWorkspaceId, targetWorkspaceId, logicalTimeSent, content) VALUES (?, ?, ?, ?)"
                            (srcId, tgtId, t :: LogicalTime, msgText)
    insertCommand sync q c conn userId srcId (Send tgtId msg)

-- TODO: Bulkify this.
expandPointerPostgres :: SyncFunc -> Queue -> Counter -> Connection -> UserId -> WorkspaceId -> Pointer -> IO ()
expandPointerPostgres sync q c conn userId workspaceId ptr = do
    t <- increment c
    enqueueAsync q $ do
        () <$ execute conn "INSERT INTO ExpandedPointers (workspaceId, pointerId, logicalTimeExpanded) VALUES (?, ?, ?) ON CONFLICT DO NOTHING"
                            (workspaceId, ptr, t :: LogicalTime)
    insertCommand sync q c conn userId workspaceId (View ptr)

nextPointerPostgres :: SyncFunc -> Queue -> Counter -> Connection -> IO Pointer
nextPointerPostgres sync q c conn = do
    sync $ do
        [Only lastPointerId] <- query_ conn "SELECT MAX(id) FROM Pointers"
        return $! maybe 0 succ lastPointerId

createPointersPostgres :: SyncFunc -> Queue -> Counter -> Connection -> PointerEnvironment -> IO ()
createPointersPostgres sync q c conn env = do
    enqueueAsync q $
        () <$ executeMany conn "INSERT INTO Pointers (id, content) VALUES (?, ?)" (M.assocs (fmap (toText . messageToBuilderDB) env))

remapPointersPostgres :: SyncFunc -> Queue -> Counter -> Connection -> PointerRemapping -> IO ()
remapPointersPostgres sync q c conn mapping = do
    enqueueAsync q $
        () <$ executeMany conn "INSERT INTO Pointers (id, content) \
                               \SELECT m.new, p.content \
                               \FROM Pointers p \
                               \INNER JOIN (VALUES (?, ?)) m(new, old) ON m.old = p.id" (M.assocs mapping)

-- NOT CACHEABLE
pendingQuestionsPostgres :: SyncFunc -> Queue -> Counter -> Connection -> WorkspaceId -> IO [WorkspaceId]
pendingQuestionsPostgres sync q c conn workspaceId = do
    sync $ do
        subquestions <- query conn "SELECT w.id \
                                   \FROM Workspaces w \
                                   \LEFT OUTER JOIN Answers a ON a.workspaceId = w.id \
                                   \WHERE w.parentWorkspaceId = ? \
                                   \  AND a.answer IS NULL ORDER BY logicalTime ASC" (Only workspaceId)
        return $ map (\(Only qId) -> qId) subquestions

-- TODO: Maybe maintain a cache of workspaces.
-- NOT CACHEABLE but the components should be. Cacheable if answered, for now at least.
getWorkspacePostgres :: SyncFunc -> Queue -> Counter -> Connection -> WorkspaceId -> IO Workspace
getWorkspacePostgres sync q c conn workspaceId = do
    sync $ do
        withTransaction conn $ do
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
allWorkspacesPostgres :: SyncFunc -> Queue -> Counter -> Connection -> IO (M.Map WorkspaceId Workspace)
allWorkspacesPostgres sync q c conn = do
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
            let messageMap = M.fromListWith (++) $ map (\(i, m) -> (i, [parseMessageUnsafe m])) messages
                subquestionsMap = M.fromListWith (++) $ map (\(i, qId, q, ma) -> (i, [(qId, parseMessageUnsafe q, fmap parseMessageUnsafeDB ma)])) subquestions
                expandedMap = M.fromListWith M.union $ map (\(i, p, m) -> (i, M.singleton p (parseMessageUnsafe' p m))) expanded
            return $ M.fromList $ map (\(i, p, t, q) -> (i, Workspace {
                                                                identity = i,
                                                                parentId = p,
                                                                question = parseMessageUnsafeDB q,
                                                                subQuestions = maybe [] id $ M.lookup i subquestionsMap,
                                                                messageHistory = maybe [] id $ M.lookup i messageMap,
                                                                expandedPointers = maybe M.empty id $ M.lookup i expandedMap,
                                                                time = Time t })) workspaces

-- NOT CACHEABLE
getNextWorkspacePostgres :: SyncFunc -> Queue -> Counter -> Connection -> IO (Maybe WorkspaceId)
getNextWorkspacePostgres sync q c conn = do
    sync $ do
        -- This gets a workspace that doesn't currently have an answer.
        result <- query_ conn "SELECT w.id \
                              \FROM Workspaces w \
                              \WHERE NOT EXISTS(SELECT * FROM Answers a WHERE a.workspaceId = w.id) ORDER BY w.logicalTime DESC LIMIT 1"
        case result of
            [] -> return Nothing
            [Only workspaceId] -> return (Just workspaceId)
