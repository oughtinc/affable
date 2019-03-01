{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module SqliteSchedulerContext ( makeSqliteSchedulerContext ) where
import Data.Int ( Int64 ) -- base
import qualified Data.Map as M -- containers
import Data.Maybe ( mapMaybe ) -- base
import Data.String ( fromString ) -- base
import Data.Text ( Text ) -- text
import Data.Traversable ( mapAccumL ) -- base
import Database.SQLite.Simple ( Connection, Only(..), NamedParam(..),
                                query, query_, execute, execute_, executeMany, executeNamed, lastInsertRowId, withTransaction ) -- sqlite-simple

import Command ( Command(..), commandToBuilder )
import Message ( Message(..), Pointer, PointerEnvironment, PointerRemapping, normalizeMessage, generalizeMessage,
                 messageToBuilder, messageToBuilderDB, parseMessageUnsafe, parseMessageUnsafe', parseMessageUnsafeDB,
                 canonicalizeMessage, renumberMessage', boundPointers )
import Scheduler ( SchedulerContext(..), Event, UserId, SessionId )
import qualified Scheduler as Event ( Event(..) )
import Time ( Time(..), LogicalTime )
import Util ( toText, Lock, newLock, withLock )
import Workspace ( Workspace(..), WorkspaceId )

makeSqliteSchedulerContext :: Connection -> IO (SchedulerContext (Connection, Lock))
makeSqliteSchedulerContext conn = do
    lock <- newLock
    return $
        SchedulerContext {
            createInitialWorkspace = createInitialWorkspaceSqlite lock conn,
            newSession = newSessionSqlite lock conn,
            createWorkspace = createWorkspaceSqlite lock conn,
            sendAnswer = sendAnswerSqlite lock conn,
            sendMessage = sendMessageSqlite lock conn,
            expandPointer = expandPointerSqlite lock conn,
            pendingQuestions = pendingQuestionsSqlite lock conn,
            getWorkspace = getWorkspaceSqlite lock conn,
            allWorkspaces = allWorkspacesSqlite lock conn,
            getNextWorkspace = getNextWorkspaceSqlite lock conn,
            labelMessage = labelMessageSqlite lock conn,
            normalize = insertMessagePointers lock conn,
            canonicalizeEvents = canonicalizeEventsSqlite lock conn,
            generalize = insertGeneralizedMessagePointers lock conn,
            dereference = dereferenceSqlite lock conn,
            reifyWorkspace = reifyWorkspaceSqlite lock conn,
            extraContent = (conn, lock)
        }

-- NOT CACHEABLE
reifyWorkspaceSqlite :: Lock -> Connection -> WorkspaceId -> IO Message
reifyWorkspaceSqlite lock conn workspaceId = do
    workspaces <- withLock lock $ do
        withTransaction conn $ do
            execute_ conn "CREATE TEMP TABLE IF NOT EXISTS Descendants ( id INTEGER PRIMRY KEY ASC )"
            execute_ conn "DELETE FROM Descendants"
            executeNamed conn "WITH RECURSIVE ds(id) AS ( \
                              \     VALUES (:root) \
                              \ UNION ALL \
                              \     SELECT w.id FROM Workspaces w INNER JOIN ds ON w.parentWorkspaceId = ds.id \
                              \) INSERT INTO Descendants ( id ) SELECT id FROM ds" [":root" := workspaceId]
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
                                        \ORDER BY p.id ASC, q.id DESC"
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

-- TODO: This could also be done in a way to better reuse existing pointers rather than make new pointers.
-- TODO: Should the expanded pointers be indicated some way?
workspaceToMessage :: M.Map WorkspaceId Workspace -> WorkspaceId -> Message
workspaceToMessage workspaces workspaceId = go (M.lookup workspaceId workspaces)
    where go (Just workspace) | null subQs && null msgs = Structured [Text "Question: ", question workspace]
                              | null subQs = Structured (Text "Question: "
                                                        : question workspace
                                                        : Text " Messages: 1. "
                                                        : msgs)
                              | null msgs = Structured (Text "Question: "
                                                       : question workspace
                                                       : Text " Subquestions: 1. "
                                                       : subQs)
                              | otherwise =  Structured (Text "Question: "
                                                        : question workspace
                                                        : Text " Messages: 1. "
                                                        : msgs
                                                        ++ (Text " Subquestions: 1. "
                                                           : subQs))
            where subQs = goSub 1 (subQuestions workspace)
                  msgs = goMsg 1 (messageHistory workspace)
                  goSub !i [] = []
                  goSub i ((_, _, Nothing):qs) = goSub i qs
                  goSub 1 ((wsId, _, Just a):qs) -- To avoid [Text "...", Text "..."]
                    = go (M.lookup wsId workspaces):Text " Answer:":a:goSub 2 qs
                  goSub i ((wsId, _, Just a):qs)
                    = Text (fromString (' ':show i ++ ". ")):go (M.lookup wsId workspaces):Text " Answer:":a:goSub (i+1) qs
                  goMsg !i [] = []
                  goMsg 1 (m:ms) = m:goMsg 2 ms
                  goMsg i (m:ms) = Text (fromString (' ':show i ++ ". ")):m:goMsg (i+1) ms


renumberEvent :: PointerRemapping -> Event -> Event
renumberEvent mapping (Event.Create m) = Event.Create (renumberMessage' mapping m)
renumberEvent mapping (Event.Answer m) = Event.Answer (renumberMessage' mapping m)
renumberEvent mapping (Event.Send loc m) = Event.Send loc (renumberMessage' mapping m)
renumberEvent mapping m = m

eventMessage :: Event -> Maybe Message
eventMessage (Event.Create m) = Just m
eventMessage (Event.Answer m) = Just m
eventMessage (Event.Send _ m) = Just m
eventMessage _ = Nothing

-- TODO: Rename.
-- This takes a Message from the user where the LabeledStructures represent binding forms and produces
-- a Message and pointer state that corresponds to that binding structure.
canonicalizeEventsSqlite :: Lock -> Connection -> [Event] -> IO [Event]
canonicalizeEventsSqlite lock conn evts = do
    case traverse boundPointers $ mapMaybe eventMessage evts of
        Right envs -> do
            let !env = M.unions envs -- TODO: XXX There could be re-use of bound variables, so this needs to be handled in a smarter manner.
            withLock lock $ do
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
                    return evts'
        -- Left p -> return (Left p)
        -- TODO: XXX Either decide to assume evts is well-formed, and enforce it, or propagate the error.

-- TODO: Bulkify this.
-- CACHEABLE
dereferenceSqlite :: Lock -> Connection -> Pointer -> IO Message
dereferenceSqlite lock conn ptr = do
    withLock lock $ do
        [Only t] <- query conn "SELECT content FROM Pointers WHERE id = ? LIMIT 1" (Only ptr)
        return $! parseMessageUnsafe' ptr t

-- Normalize the Message, write the new pointers to the database, then return the normalized message.
insertMessagePointers :: Lock -> Connection -> Message -> IO Message
insertMessagePointers lock conn msg = do
    withLock lock $ do
        withTransaction conn $ do
            [Only lastPointerId] <- query_ conn "SELECT MAX(id) FROM Pointers"
            let (pEnv, normalizedMsg) = normalizeMessage (maybe 0 succ lastPointerId) msg
            executeMany conn "INSERT INTO Pointers (id, content) VALUES (?, ?)" (M.assocs (fmap (toText . messageToBuilderDB) pEnv))
            return normalizedMsg

insertGeneralizedMessagePointers :: Lock -> Connection -> Message -> IO Message
insertGeneralizedMessagePointers lock conn msg = do
    withLock lock $ do
        withTransaction conn $ do
            [Only lastPointerId] <- query_ conn "SELECT MAX(id) FROM Pointers"
            let (mapping, generalizedMsg) = generalizeMessage (maybe 0 succ lastPointerId) msg
            executeMany conn "INSERT INTO Pointers (id, content) SELECT ?, o.content FROM Pointers o WHERE o.id = ?" (M.assocs mapping)
            return generalizedMsg

labelMessageSqlite :: Lock -> Connection -> Message -> IO Message
labelMessageSqlite lock conn msg@(Structured ms) = do
    let !msgText = toText (messageToBuilderDB msg)
    withLock lock $ do
        execute conn "INSERT INTO Pointers (content) VALUES (?)" [msgText]
        p <- fromIntegral <$> lastInsertRowId conn
        return (LabeledStructured p ms)
labelMessageSqlite lock conn msg = do
    let !msgText = toText (messageToBuilderDB msg)
    withLock lock $ do
        execute conn "INSERT INTO Pointers (content) VALUES (?)" [msgText]
        p <- fromIntegral <$> lastInsertRowId conn
        return (LabeledStructured p [msg])

insertCommand :: Lock -> Connection -> UserId -> WorkspaceId -> Command -> IO ()
insertCommand lock conn userId workspaceId cmd = do
    let !cmdText = toText (commandToBuilder cmd)
    withLock lock $ do
        mt <- query conn "SELECT commandTime FROM Commands WHERE workspaceId = ? ORDER BY commandTime DESC LIMIT 1" (Only workspaceId)
        let t = case mt of [] -> 0; [Only t'] -> t'+1
        executeNamed conn "INSERT INTO Commands (workspaceId, commandTime, userId, command) VALUES (:workspace, :time, :userId, :cmd)" [
                            ":workspace" := workspaceId,
                            ":time" := (t :: Int64),
                            ":userId" := userId,
                            ":cmd" := cmdText]

createInitialWorkspaceSqlite :: Lock -> Connection -> IO WorkspaceId
createInitialWorkspaceSqlite lock conn = do
    let msg = Text "What is your question?"
    msg' <- labelMessageSqlite lock conn msg
    let !msgText = toText (messageToBuilder msg)
        !msgText' = toText (messageToBuilder msg')
    withLock lock $ do
        executeNamed conn "INSERT INTO Workspaces (logicalTime, parentWorkspaceId, questionAsAsked, questionAsAnswered) \
                          \VALUES (:time, :parent, :questionAsAsked, :questionAsAnswered)" [
                            ":time" := (0 :: LogicalTime),
                            ":parent" := (Nothing :: Maybe WorkspaceId),
                            ":questionAsAsked" := msgText,
                            ":questionAsAnswered" := msgText']
        lastInsertRowId conn

newSessionSqlite :: Lock -> Connection -> Maybe SessionId -> IO SessionId
newSessionSqlite lock conn Nothing = do
    withLock lock $ do
        execute_ conn "INSERT INTO Sessions DEFAULT VALUES"
        lastInsertRowId conn
newSessionSqlite lock conn (Just sessionId) = do
    withLock lock $ do
        execute conn "INSERT OR IGNORE INTO Sessions VALUES (?)" (Only sessionId)
        return sessionId

createWorkspaceSqlite :: Lock -> Connection -> Bool -> UserId -> WorkspaceId -> Message -> Message -> IO WorkspaceId
createWorkspaceSqlite lock conn doNormalize userId workspaceId qAsAsked qAsAnswered = do
    qAsAnswered' <- if doNormalize then insertMessagePointers lock conn qAsAnswered else return qAsAnswered
    let !qAsAskedText = toText (messageToBuilder qAsAsked)
        !qAsAnsweredText = toText (messageToBuilder qAsAnswered')
    newWorkspaceId <- withLock lock $ do
        executeNamed conn "INSERT INTO Workspaces (logicalTime, parentWorkspaceId, questionAsAsked, questionAsAnswered) VALUES (:time, :parent, :asAsked, :asAnswered)" [
                            ":time" := (0 :: LogicalTime), -- TODO
                            ":parent" := Just workspaceId,
                            ":asAsked" := qAsAskedText,
                            ":asAnswered" := qAsAnsweredText]
        lastInsertRowId conn
    insertCommand lock conn userId workspaceId (Ask qAsAsked)
    return newWorkspaceId

sendAnswerSqlite :: Lock -> Connection -> Bool -> UserId -> WorkspaceId -> Message -> IO ()
sendAnswerSqlite lock conn doNormalize userId workspaceId msg = do
    msg' <- if doNormalize then insertMessagePointers lock conn msg else return msg
    let !msgText = toText (messageToBuilder msg')
    withLock lock $ do
        -- TODO: XXX If we revisit, and thus change an answer, this will need to be an INSERT OR REPLACE or we'll need to start
        -- actually using this time parameter. If this is all that is changed, then we'll get a model of edits where we see
        -- the following questions upon return, possibly referring to pointers in an answer that no longer exist.
        executeNamed conn "INSERT OR REPLACE INTO Answers (workspaceId, logicalTimeAnswered, answer) VALUES (:workspace, :time, :answer)" [
                            ":workspace" := workspaceId,
                            ":time" := (0 :: LogicalTime), -- TODO
                            ":answer" := msgText]
    insertCommand lock conn userId workspaceId (Reply msg)

sendMessageSqlite :: Lock -> Connection -> Bool -> UserId -> WorkspaceId -> WorkspaceId -> Message -> IO ()
sendMessageSqlite lock conn doNormalize userId srcId tgtId msg = do
    msg' <- if doNormalize then insertMessagePointers lock conn msg else return msg
    let !msgText = toText (messageToBuilder msg')
    withLock lock $ do
        executeNamed conn "INSERT INTO Messages (sourceWorkspaceId, targetWorkspaceId, logicalTimeSent, content) VALUES (:source, :target, :time, :content)" [
                            ":source" := srcId,
                            ":target" := tgtId,
                            ":time" := (0 :: LogicalTime), -- TODO
                            ":content" := msgText]
    insertCommand lock conn userId srcId (Send (fromIntegral tgtId) msg)

-- TODO: Bulkify this.
expandPointerSqlite :: Lock -> Connection -> UserId -> WorkspaceId -> Pointer -> IO ()
expandPointerSqlite lock conn userId workspaceId ptr = do
    withLock lock $ do
        executeNamed conn "INSERT OR IGNORE INTO ExpandedPointers (workspaceId, pointerId, logicalTimeExpanded) VALUES (:workspace, :pointer, :time)" [
                            ":workspace" := workspaceId,
                            ":pointer" := ptr,
                            ":time" := (0 :: LogicalTime)] -- TODO
    insertCommand lock conn userId workspaceId (View ptr)

-- NOT CACHEABLE
pendingQuestionsSqlite :: Lock -> Connection -> WorkspaceId -> IO [WorkspaceId]
pendingQuestionsSqlite lock conn workspaceId = do
    withLock lock $ do
        subquestions <- query conn "SELECT w.id \
                                   \FROM Workspaces w \
                                   \LEFT OUTER JOIN Answers a ON a.workspaceId = w.id \
                                   \WHERE w.parentWorkspaceId = ? \
                                   \  AND a.answer IS NULL ORDER BY id ASC" (Only workspaceId)
        return $ map (\(Only qId) -> qId) subquestions

-- TODO: Maybe maintain a cache of workspaces.
-- NOT CACHEABLE but the components should be. Cacheable if answered, for now at least.
getWorkspaceSqlite :: Lock -> Connection -> WorkspaceId -> IO Workspace
getWorkspaceSqlite lock conn workspaceId = do
    withLock lock $ do
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
                                       \ORDER BY w.id ASC" (Only workspaceId)
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
allWorkspacesSqlite :: Lock -> Connection -> IO (M.Map WorkspaceId Workspace)
allWorkspacesSqlite lock conn = do
    withLock lock $ do
        withTransaction conn $ do
            workspaces <- query_ conn "SELECT id, parentWorkspaceId, logicalTime, questionAsAnswered \
                                      \FROM Workspaces"
            messages <- query_ conn "SELECT targetWorkspaceId, content FROM Messages" -- TODO: ORDER
            subquestions <- query_ conn "SELECT p.id, q.id, q.questionAsAsked, a.answer \
                                        \FROM Workspaces p \
                                        \INNER JOIN Workspaces q ON q.parentWorkspaceId = p.id \
                                        \LEFT OUTER JOIN Answers a ON q.id = a.workspaceId \
                                        \ORDER BY p.id ASC, q.id DESC"
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
getNextWorkspaceSqlite :: Lock -> Connection -> IO (Maybe WorkspaceId)
getNextWorkspaceSqlite lock conn = do
    withLock lock $ do
        -- This gets a workspace that doesn't currently have an answer.
        result <- query_ conn "SELECT w.id \
                              \FROM Workspaces w \
                              \WHERE NOT EXISTS(SELECT * FROM Answers a WHERE a.workspaceId = w.id) ORDER BY w.id DESC LIMIT 1"
        case result of
            [] -> return Nothing
            [Only workspaceId] -> return (Just workspaceId)
