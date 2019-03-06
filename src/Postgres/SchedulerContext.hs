{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module Postgres.SchedulerContext ( makePostgresSchedulerContext ) where
import Data.Int ( Int64 ) -- base
import qualified Data.Map as M -- containers
import Data.Maybe ( mapMaybe ) -- base
import Data.Text ( Text ) -- text
import Data.Traversable ( mapAccumL ) -- base
import Database.PostgreSQL.Simple ( Connection, Only(..),
                                    query, query_, execute, execute_, executeMany, withTransaction ) -- postgresql-simple

import Command ( Command(..), commandToBuilder )
import Message ( Message(..), Pointer, PointerEnvironment, PointerRemapping, normalizeMessage, generalizeMessage,
                 messageToBuilder, messageToBuilderDB, parseMessageUnsafe, parseMessageUnsafe', parseMessageUnsafeDB,
                 canonicalizeMessage, boundPointers )
import Scheduler ( SchedulerContext(..), Event, UserId, SessionId, workspaceToMessage, eventMessage, renumberEvent )
import Time ( Time(..), LogicalTime )
import Util ( toText, Counter, newCounter, increment, Queue, newQueue, enqueueAsync, enqueueSync )
import Workspace ( Workspace(..), WorkspaceId, newWorkspaceId )

makePostgresSchedulerContext :: Connection -> IO (SchedulerContext (Connection, Queue))
makePostgresSchedulerContext conn = do
    q <- newQueue
    c <- newCounter 0 -- TODO: XXX Initialize from database.
    return $
        SchedulerContext {
            createInitialWorkspace = createInitialWorkspacePostgres q c conn,
            newSession = newSessionPostgres q c conn,
            createWorkspace = createWorkspacePostgres q c conn,
            sendAnswer = sendAnswerPostgres q c conn,
            sendMessage = sendMessagePostgres q c conn,
            expandPointer = expandPointerPostgres q c conn,
            createPointers = createPointersPostgres q c conn,
            pendingQuestions = pendingQuestionsPostgres q c conn,
            getWorkspace = getWorkspacePostgres q c conn,
            allWorkspaces = allWorkspacesPostgres q c conn,
            getNextWorkspace = getNextWorkspacePostgres q c conn,
            labelMessage = labelMessagePostgres q c conn,
            normalizeEnv = insertMessagePointers q c conn,
            canonicalizeEventsEnv = canonicalizeEventsPostgres q c conn,
            generalizeEnv = insertGeneralizedMessagePointers q c conn,
            dereference = dereferencePostgres q c conn,
            reifyWorkspace = reifyWorkspacePostgres q c conn,
            extraContent = (conn, q)
        }

-- NOT CACHEABLE
reifyWorkspacePostgres :: Queue -> Counter -> Connection -> WorkspaceId -> IO Message
reifyWorkspacePostgres q c conn workspaceId = do
    workspaces <- enqueueSync q $ do
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

-- This takes a Message from the user where the LabeledStructures represent binding forms and produces
-- a Message and pointer state that corresponds to that binding structure.
canonicalizeEventsPostgres :: Queue -> Counter -> Connection -> [Event] -> IO (PointerEnvironment, [Event])
canonicalizeEventsPostgres q c conn evts = do
    case traverse boundPointers $ mapMaybe eventMessage evts of
        Right envs -> do
            let !env = M.unions envs -- TODO: XXX There could be re-use of bound variables, so this needs to be handled in a smarter manner.
            enqueueSync q $ do
                withTransaction conn $ do
                    [Only lastPointerId] <- query_ conn "SELECT MAX(id) FROM Pointers"
                    let !firstPointerId = maybe 0 succ lastPointerId
                    let !topPointerId = firstPointerId + M.size env - 1
                    let !mapping = M.fromList (zip (M.keys env) [firstPointerId ..])
                    let !evts' = map (renumberEvent mapping) evts
                    let canonicalizeMsg !top msg = (top + M.size pEnv, pEnv)
                            where (pEnv, _) = canonicalizeMessage top msg
                    let !pEnv = M.unions $ snd $ mapAccumL canonicalizeMsg (topPointerId + 1) (mapMaybe eventMessage evts')
                    executeMany conn "INSERT INTO Pointers (id, content) VALUES (?, ?)" (M.assocs (fmap (toText . messageToBuilderDB) pEnv))
                    return (pEnv, evts')
        -- Left p -> return (Left p)
        -- TODO: XXX Either decide to assume evts is well-formed, and enforce it, or propagate the error.

-- TODO: Bulkify this.
-- CACHEABLE
dereferencePostgres :: Queue -> Counter -> Connection -> Pointer -> IO Message
dereferencePostgres q c conn ptr = do
    enqueueSync q $ do
        [Only t] <- query conn "SELECT content FROM Pointers WHERE id = ? LIMIT 1" (Only ptr)
        return $! parseMessageUnsafe' ptr t

-- Normalize the Message, write the new pointers to the database, then return the normalized message.
insertMessagePointers :: Queue -> Counter -> Connection -> Message -> IO (PointerEnvironment, Message)
insertMessagePointers q c conn msg = do
    enqueueSync q $ do
        withTransaction conn $ do
            [Only lastPointerId] <- query_ conn "SELECT MAX(id) FROM Pointers"
            let (pEnv, normalizedMsg) = normalizeMessage (maybe 0 succ lastPointerId) msg
            executeMany conn "INSERT INTO Pointers (id, content) VALUES (?, ?)" (M.assocs (fmap (toText . messageToBuilderDB) pEnv))
            return (pEnv, normalizedMsg)

insertGeneralizedMessagePointers :: Queue -> Counter -> Connection -> Message -> IO (PointerRemapping, Message)
insertGeneralizedMessagePointers q c conn msg = do
    enqueueSync q $ do
        withTransaction conn $ do
            [Only lastPointerId] <- query_ conn "SELECT MAX(id) FROM Pointers"
            let (mapping, generalizedMsg) = generalizeMessage (maybe 0 succ lastPointerId) msg
            executeMany conn "INSERT INTO Pointers (id, content) \
                             \SELECT t.new, o.content FROM (VALUES (?, ?)) t(new, old) \
                             \INNER JOIN Pointers o ON o.id = t.old" (M.assocs mapping)
            return (mapping, generalizedMsg)

labelMessagePostgres :: Queue -> Counter -> Connection -> Message -> IO Message
labelMessagePostgres q c conn msg@(Structured ms) = do
    let !msgText = toText (messageToBuilderDB msg)
    enqueueSync q $ do
        withTransaction conn $ do
            [Only lastPointerId] <- query_ conn "SELECT MAX(id) FROM Pointers"
            [Only p] <- query conn "INSERT INTO Pointers (id, content) VALUES (?, ?) RETURNING id" (maybe 0 succ lastPointerId :: Pointer, msgText)
            return (LabeledStructured p ms)
labelMessagePostgres q c conn msg = do
    let !msgText = toText (messageToBuilderDB msg)
    enqueueSync q $ do
        withTransaction conn $ do
            [Only lastPointerId] <- query_ conn "SELECT MAX(id) FROM Pointers"
            [Only p] <- query conn "INSERT INTO Pointers (id, content) VALUES (?, ?) RETURNING id" (maybe 0 succ lastPointerId :: Pointer, msgText)
            return (LabeledStructured p [msg])

insertCommand :: Queue -> Counter -> Connection -> UserId -> WorkspaceId -> Command -> IO ()
insertCommand q c conn userId workspaceId cmd = do
    let !cmdText = toText (commandToBuilder cmd)
    enqueueAsync q $ do
        mt <- query conn "SELECT commandTime FROM Commands WHERE workspaceId = ? ORDER BY commandTime DESC LIMIT 1" (Only workspaceId)
        let t = case mt of [] -> 0; [Only t'] -> t'+1
        () <$ execute conn "INSERT INTO Commands (workspaceId, commandTime, userId, command) VALUES (?, ?, ?, ?)"
                            (workspaceId, t :: Int64, userId, cmdText)

createInitialWorkspacePostgres :: Queue -> Counter -> Connection -> IO WorkspaceId
createInitialWorkspacePostgres q c conn = do
    let msg = Text "What is your question?"
    msg' <- labelMessagePostgres q c conn msg
    let !msgText = toText (messageToBuilder msg)
        !msgText' = toText (messageToBuilder msg')
    workspaceId <- newWorkspaceId
    t <- increment c
    enqueueAsync q $ do
        () <$ execute conn "INSERT INTO Workspaces (id, logicalTime, parentWorkspaceId, questionAsAsked, questionAsAnswered) \
                           \VALUES (?, ?, ?, ?, ?)"
                             (workspaceId, t :: LogicalTime, Nothing :: Maybe WorkspaceId, msgText, msgText')
    return workspaceId

newSessionPostgres :: Queue -> Counter -> Connection -> Maybe SessionId -> IO SessionId
newSessionPostgres q c conn Nothing = do
    enqueueSync q $ do
        [Only sessionId] <- query_ conn "INSERT INTO Sessions DEFAULT VALUES RETURNING sessionId"
        return sessionId
newSessionPostgres q c conn (Just sessionId) = do
    enqueueSync q $ do
        execute conn "INSERT INTO Sessions VALUES (?) ON CONFLICT DO NOTHING" (Only sessionId)
        return sessionId

createWorkspacePostgres :: Queue -> Counter -> Connection -> Bool -> UserId -> WorkspaceId -> Message -> Message -> IO WorkspaceId
createWorkspacePostgres q c conn doNormalize userId workspaceId qAsAsked qAsAnswered = do
    qAsAnswered' <- if doNormalize then snd <$> insertMessagePointers q c conn qAsAnswered else return qAsAnswered
    let !qAsAskedText = toText (messageToBuilder qAsAsked)
        !qAsAnsweredText = toText (messageToBuilder qAsAnswered')
    wsId <- newWorkspaceId
    t <- increment c
    enqueueAsync q $ do
        () <$ execute conn "INSERT INTO Workspaces (id, logicalTime, parentWorkspaceId, questionAsAsked, questionAsAnswered) \
                           \VALUES (?, ?, ?, ?, ?)"
                            (wsId, t :: LogicalTime, Just workspaceId, qAsAskedText, qAsAnsweredText)
    insertCommand q c conn userId workspaceId (Ask qAsAsked)
    return wsId

sendAnswerPostgres :: Queue -> Counter -> Connection -> Bool -> UserId -> WorkspaceId -> Message -> IO ()
sendAnswerPostgres q c conn doNormalize userId workspaceId msg = do
    msg' <- if doNormalize then snd <$> insertMessagePointers q c conn msg else return msg
    let !msgText = toText (messageToBuilder msg')
    t <- increment c
    enqueueAsync q $ do
        () <$ execute conn "INSERT INTO Answers (workspaceId, logicalTimeAnswered, answer) VALUES (?, ?, ?) \
                           \ON CONFLICT(workspaceId) DO UPDATE SET logicalTimeAnswered = excluded.logicalTimeAnswered, answer = excluded.answer"
                            (workspaceId, t :: LogicalTime, msgText)
    insertCommand q c conn userId workspaceId (Reply msg)

sendMessagePostgres :: Queue -> Counter -> Connection -> Bool -> UserId -> WorkspaceId -> WorkspaceId -> Message -> IO ()
sendMessagePostgres q c conn doNormalize userId srcId tgtId msg = do
    msg' <- if doNormalize then snd <$> insertMessagePointers q c conn msg else return msg
    let !msgText = toText (messageToBuilder msg')
    t <- increment c
    enqueueAsync q $ do
        () <$ execute conn "INSERT INTO Messages (sourceWorkspaceId, targetWorkspaceId, logicalTimeSent, content) VALUES (?, ?, ?, ?)"
                            (srcId, tgtId, t :: LogicalTime, msgText)
    insertCommand q c conn userId srcId (Send tgtId msg)

-- TODO: Bulkify this.
expandPointerPostgres :: Queue -> Counter -> Connection -> UserId -> WorkspaceId -> Pointer -> IO ()
expandPointerPostgres q c conn userId workspaceId ptr = do
    t <- increment c
    enqueueAsync q $ do
        () <$ execute conn "INSERT INTO ExpandedPointers (workspaceId, pointerId, logicalTimeExpanded) VALUES (?, ?, ?) ON CONFLICT DO NOTHING"
                            (workspaceId, ptr, t :: LogicalTime)
    insertCommand q c conn userId workspaceId (View ptr)

createPointersPostgres :: Queue -> Counter -> Connection -> PointerEnvironment -> IO ()
createPointersPostgres q c conn env = do
    enqueueAsync q $
        () <$ executeMany conn "INSERT INTO Pointers (id, content) VALUES (?, ?)" (M.assocs (fmap (toText . messageToBuilderDB) env))

-- NOT CACHEABLE
pendingQuestionsPostgres :: Queue -> Counter -> Connection -> WorkspaceId -> IO [WorkspaceId]
pendingQuestionsPostgres q c conn workspaceId = do
    enqueueSync q $ do
        subquestions <- query conn "SELECT w.id \
                                   \FROM Workspaces w \
                                   \LEFT OUTER JOIN Answers a ON a.workspaceId = w.id \
                                   \WHERE w.parentWorkspaceId = ? \
                                   \  AND a.answer IS NULL ORDER BY logicalTime ASC" (Only workspaceId)
        return $ map (\(Only qId) -> qId) subquestions

-- TODO: Maybe maintain a cache of workspaces.
-- NOT CACHEABLE but the components should be. Cacheable if answered, for now at least.
getWorkspacePostgres :: Queue -> Counter -> Connection -> WorkspaceId -> IO Workspace
getWorkspacePostgres q c conn workspaceId = do
    enqueueSync q $ do
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
allWorkspacesPostgres :: Queue -> Counter -> Connection -> IO (M.Map WorkspaceId Workspace)
allWorkspacesPostgres q c conn = do
    enqueueSync q $ do
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
getNextWorkspacePostgres :: Queue -> Counter -> Connection -> IO (Maybe WorkspaceId)
getNextWorkspacePostgres q c conn = do
    enqueueSync q $ do
        -- This gets a workspace that doesn't currently have an answer.
        result <- query_ conn "SELECT w.id \
                              \FROM Workspaces w \
                              \WHERE NOT EXISTS(SELECT * FROM Answers a WHERE a.workspaceId = w.id) ORDER BY w.logicalTime DESC LIMIT 1"
        case result of
            [] -> return Nothing
            [Only workspaceId] -> return (Just workspaceId)
