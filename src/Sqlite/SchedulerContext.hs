{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module Sqlite.SchedulerContext ( makeSqliteSchedulerContext ) where
import Data.Int ( Int64 ) -- base
import qualified Data.Map as M -- containers
import Data.Maybe ( mapMaybe ) -- base
import Data.Text ( Text ) -- text
import Data.Traversable ( mapAccumL ) -- base
import Database.SQLite.Simple ( Connection, Only(..), NamedParam(..),
                                query, query_, execute, execute_, executeMany, executeNamed, lastInsertRowId, withTransaction ) -- sqlite-simple

import Command ( Command(..), commandToBuilder )
import Message ( Message(..), Pointer, PointerEnvironment, PointerRemapping, normalizeMessage, generalizeMessage,
                 messageToBuilder, messageToBuilderDB, parseMessageUnsafe, parseMessageUnsafe', parseMessageUnsafeDB,
                 canonicalizeMessage, boundPointers )
import Scheduler ( SchedulerContext(..), Event, UserId, SessionId,
                   userIdToBuilder, sessionIdToBuilder, newSessionId, workspaceToMessage, eventMessage, renumberEvent )
import Time ( Time(..), LogicalTime )
import Util ( toText, Counter, newCounter, increment, Queue, newQueue, enqueueAsync, enqueueSync )
import Workspace ( Workspace(..), WorkspaceId, newWorkspaceId, workspaceIdFromText, workspaceIdToBuilder )

makeSqliteSchedulerContext :: Connection -> IO (SchedulerContext (Connection, Queue))
makeSqliteSchedulerContext conn = do
    q <- newQueue
    c <- newCounter 0 -- TODO: XXX Initialize from database.
    return $
        SchedulerContext {
            createInitialWorkspace = createInitialWorkspaceSqlite q c conn,
            newSession = newSessionSqlite q c conn,
            createWorkspace = createWorkspaceSqlite q c conn,
            sendAnswer = sendAnswerSqlite q c conn,
            sendMessage = sendMessageSqlite q c conn,
            expandPointer = expandPointerSqlite q c conn,
            createPointers = createPointersSqlite q c conn,
            pendingQuestions = pendingQuestionsSqlite q c conn,
            getWorkspace = getWorkspaceSqlite q c conn,
            allWorkspaces = allWorkspacesSqlite q c conn,
            getNextWorkspace = getNextWorkspaceSqlite q c conn,
            labelMessage = labelMessageSqlite q c conn,
            normalizeEnv = insertMessagePointers q c conn,
            canonicalizeEventsEnv = canonicalizeEventsSqlite q c conn,
            generalizeEnv = insertGeneralizedMessagePointers q c conn,
            dereference = dereferenceSqlite q c conn,
            reifyWorkspace = reifyWorkspaceSqlite q c conn,
            extraContent = (conn, q)
        }

-- NOT CACHEABLE
reifyWorkspaceSqlite :: Queue -> Counter -> Connection -> WorkspaceId -> IO Message
reifyWorkspaceSqlite q c conn workspaceId = do
    let !wsIdText = toText (workspaceIdToBuilder workspaceId)
    workspaces <- enqueueSync q $ do
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

-- This takes a Message from the user where the LabeledStructures represent binding forms and produces
-- a Message and pointer state that corresponds to that binding structure.
canonicalizeEventsSqlite :: Queue -> Counter -> Connection -> [Event] -> IO (PointerEnvironment, [Event])
canonicalizeEventsSqlite q c conn evts = do
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
dereferenceSqlite :: Queue -> Counter -> Connection -> Pointer -> IO Message
dereferenceSqlite q c conn ptr = do
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
            executeMany conn "INSERT INTO Pointers (id, content) SELECT ?, o.content FROM Pointers o WHERE o.id = ?" (M.assocs mapping)
            return (mapping, generalizedMsg)

labelMessageSqlite :: Queue -> Counter -> Connection -> Message -> IO Message
labelMessageSqlite q c conn msg@(Structured ms) = do
    let !msgText = toText (messageToBuilderDB msg)
    enqueueSync q $ do
        execute conn "INSERT INTO Pointers (content) VALUES (?)" [msgText]
        p <- fromIntegral <$> lastInsertRowId conn
        return (LabeledStructured p ms)
labelMessageSqlite q c conn msg = do
    let !msgText = toText (messageToBuilderDB msg)
    enqueueSync q $ do
        execute conn "INSERT INTO Pointers (content) VALUES (?)" [msgText]
        p <- fromIntegral <$> lastInsertRowId conn
        return (LabeledStructured p [msg])

insertCommand :: Queue -> Counter -> Connection -> UserId -> WorkspaceId -> Command -> IO ()
insertCommand q c conn userId workspaceId cmd = do
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

createInitialWorkspaceSqlite :: Queue -> Counter -> Connection -> IO WorkspaceId
createInitialWorkspaceSqlite q c conn = do
    let msg = Text "What is your question?"
    msg' <- labelMessageSqlite q c conn msg
    let !msgText = toText (messageToBuilder msg)
        !msgText' = toText (messageToBuilder msg')
    workspaceId <- newWorkspaceId
    let !wsIdText = toText (workspaceIdToBuilder workspaceId)
    t <- increment c
    enqueueAsync q $ do
        executeNamed conn "INSERT INTO Workspaces (id, logicalTime, parentWorkspaceId, questionAsAsked, questionAsAnswered) \
                          \VALUES (:id, :time, :parent, :questionAsAsked, :questionAsAnswered)" [
                            ":id" := wsIdText,
                            ":time" := (t :: LogicalTime),
                            ":parent" := (Nothing :: Maybe Text),
                            ":questionAsAsked" := msgText,
                            ":questionAsAnswered" := msgText']
    return workspaceId

newSessionSqlite :: Queue -> Counter -> Connection -> Maybe SessionId -> IO SessionId
newSessionSqlite q c conn Nothing = do
    sessionId <- newSessionId
    newSessionSqlite q c conn (Just sessionId)
newSessionSqlite q c conn (Just sessionId) = do
    enqueueAsync q $
        execute conn "INSERT OR IGNORE INTO Sessions (sessionId) VALUES (?)" (Only (toText (sessionIdToBuilder sessionId)))
    return sessionId

createWorkspaceSqlite :: Queue -> Counter -> Connection -> Bool -> UserId -> WorkspaceId -> Message -> Message -> IO WorkspaceId
createWorkspaceSqlite q c conn doNormalize userId workspaceId qAsAsked qAsAnswered = do
    qAsAnswered' <- if doNormalize then snd <$> insertMessagePointers q c conn qAsAnswered else return qAsAnswered
    let !qAsAskedText = toText (messageToBuilder qAsAsked)
        !qAsAnsweredText = toText (messageToBuilder qAsAnswered')
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
    insertCommand q c conn userId workspaceId (Ask qAsAsked)
    return wsId

sendAnswerSqlite :: Queue -> Counter -> Connection -> Bool -> UserId -> WorkspaceId -> Message -> IO ()
sendAnswerSqlite q c conn doNormalize userId workspaceId msg = do
    msg' <- if doNormalize then snd <$> insertMessagePointers q c conn msg else return msg
    let !msgText = toText (messageToBuilder msg')
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
    insertCommand q c conn userId workspaceId (Reply msg)

sendMessageSqlite :: Queue -> Counter -> Connection -> Bool -> UserId -> WorkspaceId -> WorkspaceId -> Message -> IO ()
sendMessageSqlite q c conn doNormalize userId srcId tgtId msg = do
    msg' <- if doNormalize then snd <$> insertMessagePointers q c conn msg else return msg
    let !msgText = toText (messageToBuilder msg')
    t <- increment c
    enqueueAsync q $ do
        executeNamed conn "INSERT INTO Messages (sourceWorkspaceId, targetWorkspaceId, logicalTimeSent, content) VALUES (:source, :target, :time, :content)" [
                            ":source" := toText (workspaceIdToBuilder srcId),
                            ":target" := toText (workspaceIdToBuilder tgtId),
                            ":time" := (t :: LogicalTime),
                            ":content" := msgText]
    insertCommand q c conn userId srcId (Send tgtId msg)

-- TODO: Bulkify this.
expandPointerSqlite :: Queue -> Counter -> Connection -> UserId -> WorkspaceId -> Pointer -> IO ()
expandPointerSqlite q c conn userId workspaceId ptr = do
    let !wsIdText = toText (workspaceIdToBuilder workspaceId)
    t <- increment c
    enqueueAsync q $ do
        executeNamed conn "INSERT OR IGNORE INTO ExpandedPointers (workspaceId, pointerId, logicalTimeExpanded) VALUES (:workspace, :pointer, :time)" [
                            ":workspace" := wsIdText,
                            ":pointer" := ptr,
                            ":time" := (t :: LogicalTime)]
    insertCommand q c conn userId workspaceId (View ptr)

createPointersSqlite :: Queue -> Counter -> Connection -> PointerEnvironment -> IO ()
createPointersSqlite q c conn env = do
    enqueueAsync q $
        executeMany conn "INSERT INTO Pointers (id, content) VALUES (?, ?)" (M.assocs (fmap (toText . messageToBuilderDB) env))

-- NOT CACHEABLE
pendingQuestionsSqlite :: Queue -> Counter -> Connection -> WorkspaceId -> IO [WorkspaceId]
pendingQuestionsSqlite q c conn workspaceId = do
    let !wsIdText = toText (workspaceIdToBuilder workspaceId)
    enqueueSync q $ do
        subquestions <- query conn "SELECT w.id \
                                   \FROM Workspaces w \
                                   \LEFT OUTER JOIN Answers a ON a.workspaceId = w.id \
                                   \WHERE w.parentWorkspaceId = ? \
                                   \  AND a.answer IS NULL ORDER BY logicalTime ASC" (Only wsIdText)
        return $ map (\(Only qId) -> workspaceIdFromText qId) subquestions

-- TODO: Maybe maintain a cache of workspaces.
-- NOT CACHEABLE but the components should be. Cacheable if answered, for now at least.
getWorkspaceSqlite :: Queue -> Counter -> Connection -> WorkspaceId -> IO Workspace
getWorkspaceSqlite q c conn workspaceId = do
    let !wsIdText = toText (workspaceIdToBuilder workspaceId)
    enqueueSync q $ do
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
allWorkspacesSqlite :: Queue -> Counter -> Connection -> IO (M.Map WorkspaceId Workspace)
allWorkspacesSqlite q c conn = do
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
getNextWorkspaceSqlite :: Queue -> Counter -> Connection -> IO (Maybe WorkspaceId)
getNextWorkspaceSqlite q c conn = do
    enqueueSync q $ do
        -- This gets a workspace that doesn't currently have an answer.
        result <- query_ conn "SELECT w.id \
                              \FROM Workspaces w \
                              \WHERE NOT EXISTS(SELECT * FROM Answers a WHERE a.workspaceId = w.id) ORDER BY w.logicalTime DESC LIMIT 1"
        case result of
            [] -> return Nothing
            [Only workspaceId] -> return (Just $ workspaceIdFromText workspaceId)
