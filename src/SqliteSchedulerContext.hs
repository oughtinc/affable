{-# LANGUAGE OverloadedStrings #-}
module SqliteSchedulerContext ( makeSqliteSchedulerContext ) where
import Data.Int ( Int64 ) -- base
import qualified Data.Map as M -- containers
import Data.Text ( Text ) -- text
import Database.SQLite.Simple ( Connection, Only(..), NamedParam(..),
                                query, query_, execute, executeMany, executeNamed, lastInsertRowId, withTransaction ) -- sqlite-simple

import Command ( Command(..), commandToBuilder )
import Message ( Message(..), Pointer, PointerEnvironment, PointerRemapping, normalizeMessage, generalizeMessage,
                 messageToBuilder, messageToBuilderDB, parseMessageUnsafe, parseMessageUnsafe', parseMessageUnsafeDB )
import Scheduler ( SchedulerContext(..) )
import Time ( Time(..), LogicalTime )
import Util ( toText, Lock, newLock, withLock )
import Workspace ( Workspace(..), WorkspaceId )

makeSqliteSchedulerContext :: Connection -> IO (SchedulerContext (Connection, Lock))
makeSqliteSchedulerContext conn = do
    lock <- newLock
    return $
        SchedulerContext {
            createInitialWorkspace = createInitialWorkspaceSqlite lock conn,
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
            generalize = insertGeneralizedMessagePointers lock conn,
            dereference = dereferenceSqlite lock conn,
            extraContent = (conn, lock)
        }

-- TODO: Bulkify this.
dereferenceSqlite :: Lock -> Connection -> Pointer -> IO Message
dereferenceSqlite lock conn ptr = do
    withLock lock $ do
        [Only t] <- query conn "SELECT content FROM Pointers WHERE id = ? LIMIT 1" (Only ptr)
        return $! parseMessageUnsafe' ptr t

-- Normalize the Message, write the new pointers to the database, then return the normalized message.
insertMessagePointers :: Lock -> Connection -> Message -> IO Message
insertMessagePointers lock conn msg = do
    withLock lock $ do
        withTransaction conn $ do -- TODO: Need stronger transaction?
            [Only lastPointerId] <- query_ conn "SELECT MAX(id) FROM Pointers"
            let (pEnv, normalizedMsg) = normalizeMessage (maybe 0 succ lastPointerId) msg
            executeMany conn "INSERT INTO Pointers (id, content) VALUES (?, ?)" (M.assocs (fmap (toText . messageToBuilderDB) pEnv))
            return normalizedMsg

insertGeneralizedMessagePointers :: Lock -> Connection -> Message -> IO Message
insertGeneralizedMessagePointers lock conn msg = do
    withLock lock $ do
        withTransaction conn $ do -- TODO: Need stronger transaction?
            [Only lastPointerId] <- query_ conn "SELECT MAX(id) FROM Pointers"
            let (mapping, generalizedMsg) = generalizeMessage (maybe 0 succ lastPointerId) msg
            executeMany conn "INSERT INTO Pointers (id, content) SELECT ?, o.content FROM Pointers o WHERE o.id = ?" (M.assocs mapping)
            return generalizedMsg

labelMessageSqlite :: Lock -> Connection -> Message -> IO Message
labelMessageSqlite lock conn msg@(Structured ms) = do
    withLock lock $ do
        execute conn "INSERT INTO Pointers (content) VALUES (?)" [toText (messageToBuilderDB msg)]
        p <- fromIntegral <$> lastInsertRowId conn
        return (LabeledStructured p ms)
labelMessageSqlite lock conn msg = do
    withLock lock $ do
        execute conn "INSERT INTO Pointers (content) VALUES (?)" [toText (messageToBuilderDB msg)]
        p <- fromIntegral <$> lastInsertRowId conn
        return (LabeledStructured p [msg])

insertCommand :: Lock -> Connection -> WorkspaceId -> Command -> IO ()
insertCommand lock conn workspaceId cmd = do
    withLock lock $ do
        mt <- query conn "SELECT localTime FROM Commands WHERE workspaceId = ? ORDER BY localTime DESC LIMIT 1" (Only workspaceId)
        let t = case mt of [] -> 0; [Only t'] -> t'+1
        executeNamed conn "INSERT INTO Commands (workspaceId, localTime, command) VALUES (:workspace, :time, :cmd)" [
                            ":workspace" := workspaceId,
                            ":time" := (t :: Int64),
                            ":cmd" := toText (commandToBuilder cmd)]

createInitialWorkspaceSqlite :: Lock -> Connection -> IO WorkspaceId
createInitialWorkspaceSqlite lock conn = do
    withLock lock $ do
        executeNamed conn "INSERT INTO Workspaces (logicalTime, parentWorkspaceId, questionAsAsked, questionAsAnswered) VALUES (:time, :parent, :question, :question)" [
                            ":time" := (0 :: LogicalTime),
                            ":parent" := (Nothing :: Maybe WorkspaceId),
                            ":question" := ("What is your question?" :: Text)]
        lastInsertRowId conn

createWorkspaceSqlite :: Lock -> Connection -> Bool -> WorkspaceId -> Message -> Message -> IO WorkspaceId
createWorkspaceSqlite lock conn doNormalize workspaceId qAsAsked qAsAnswered = do
    qAsAnswered' <- if doNormalize then insertMessagePointers lock conn qAsAnswered else return qAsAnswered
    newWorkspaceId <- withLock lock $ do
        executeNamed conn "INSERT INTO Workspaces (logicalTime, parentWorkspaceId, questionAsAsked, questionAsAnswered) VALUES (:time, :parent, :asAsked, :asAnswered)" [
                            ":time" := (0 :: LogicalTime), -- TODO
                            ":parent" := Just workspaceId,
                            ":asAsked" := toText (messageToBuilder qAsAsked),
                            ":asAnswered" := toText (messageToBuilder qAsAnswered')]
        lastInsertRowId conn
    insertCommand lock conn workspaceId (Ask qAsAsked)
    return newWorkspaceId

sendAnswerSqlite :: Lock -> Connection -> Bool -> WorkspaceId -> Message -> IO ()
sendAnswerSqlite lock conn doNormalize workspaceId msg = do
    msg' <- if doNormalize then insertMessagePointers lock conn msg else return msg
    withLock lock $ do
        executeNamed conn "INSERT INTO Answers (workspaceId, logicalTimeAnswered, answer) VALUES (:workspace, :time, :answer)" [
                            ":workspace" := workspaceId,
                            ":time" := (0 :: LogicalTime), -- TODO
                            ":answer" := toText (messageToBuilder msg')]
    insertCommand lock conn workspaceId (Reply msg)

sendMessageSqlite :: Lock -> Connection -> Bool -> WorkspaceId -> WorkspaceId -> Message -> IO ()
sendMessageSqlite lock conn doNormalize srcId tgtId msg = do
    msg' <- if doNormalize then insertMessagePointers lock conn msg else return msg
    withLock lock $ do
        executeNamed conn "INSERT INTO Messages (sourceWorkspaceId, targetWorkspaceId, logicalTimeSent, content) VALUES (:source, :target, :time, :content)" [
                            ":source" := srcId,
                            ":target" := tgtId,
                            ":time" := (0 :: LogicalTime), -- TODO
                            ":content" := toText (messageToBuilder msg')]
    insertCommand lock conn srcId (Send (fromIntegral tgtId) msg)

-- TODO: Bulkify this.
expandPointerSqlite :: Lock -> Connection -> WorkspaceId -> Pointer -> IO ()
expandPointerSqlite lock conn workspaceId ptr = do
    withLock lock $ do
        executeNamed conn "INSERT OR IGNORE INTO ExpandedPointers (workspaceId, pointerId, logicalTimeExpanded) VALUES (:workspace, :pointer, :time)" [
                            ":workspace" := workspaceId,
                            ":pointer" := ptr,
                            ":time" := (0 :: LogicalTime)] -- TODO
    insertCommand lock conn workspaceId (View ptr)

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

allWorkspacesSqlite :: Lock -> Connection -> IO (M.Map WorkspaceId Workspace)
allWorkspacesSqlite lock conn = do
    withLock lock $ do
        withTransaction conn $ do
            workspaces <- query_ conn "SELECT id, parentWorkspaceId, logicalTime, questionAsAnswered \
                                      \FROM Workspaces"
            messages <- query_ conn "SELECT targetWorkspaceId, content FROM Messages" -- TODO: ORDER
            subquestions <- query_ conn "SELECT w.id, w.questionAsAsked, a.answer \
                                        \FROM Workspaces w \
                                        \LEFT OUTER JOIN Answers a ON w.id = a.workspaceId \
                                        \ORDER BY w.id ASC"
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

getNextWorkspaceSqlite :: Lock -> Connection -> IO (Maybe WorkspaceId)
getNextWorkspaceSqlite lock conn = do
    withLock lock $ do
        -- This gets a workspace that doesn't currently have an answer.
        result <- query conn "SELECT w.id \
                             \FROM Workspaces w \
                             \WHERE NOT EXISTS(SELECT * FROM Answers a WHERE a.workspaceId = w.id) ORDER BY w.id DESC LIMIT 1" ()
        case result of
            [] -> return Nothing
            [Only wId] -> return (Just wId)
