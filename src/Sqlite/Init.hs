{-# LANGUAGE OverloadedStrings #-}
module Sqlite.Init ( makeSqliteDatabaseContext ) where
import Data.Foldable ( forM_ ) -- base
import qualified Data.Map as M -- containers
import qualified Data.Set as S -- containers
import qualified Data.Text as T  -- text
import qualified Data.Text.IO as T  -- text
import Database.SQLite.Simple ( Connection, execute_, executeMany, query_ ) -- sqlite-simple

import AutoScheduler ( AutoSchedulerContext, processIdFromText )
import Completions ( CompletionContext )
import DatabaseContext ( DatabaseContext(..), Snapshot(..) )
import Exp ( Konts(..), Name(..), parseKont1UnsafeDB, parseFunEnv, parseVarEnv, expFromDB )
import Message ( messageToBuilderDB, messageToPattern, parseMessageUnsafe, parseMessageUnsafe', parseMessageUnsafeDB,
                 parsePatternsUnsafe )
import Primitive ( primitives )
import Scheduler ( SchedulerContext, sessionIdFromText )
import Sqlite.AutoSchedulerContext ( makeSqliteAutoSchedulerContext )
import Sqlite.CompletionContext ( makeSqliteCompletionContext )
import Sqlite.SchedulerContext ( makeSqliteSchedulerContext )
import Time ( Time(..) )
import Util ( Queue, newQueue, closeQueue, toText, parseUnsafe )
import Workspace ( Workspace(..), workspaceIdFromText )

makeSqliteDatabaseContext :: Connection -> IO (DatabaseContext (Connection, Queue))
makeSqliteDatabaseContext conn = do
    q <- newQueue
    return $ DatabaseContext {
                initDB = do initDBSqlite conn; initPrimitivesSqlite conn,
                closeDB = closeQueue q,
                primitivesToHaskell = primitivesToHaskellSqlite conn,
                snapshot = snapshotSqlite conn,
                makeSchedulerContext = makeSqliteSchedulerContext q conn,
                makeAutoSchedulerContext = makeSqliteAutoSchedulerContext,
                makeCompletionContext = makeSqliteCompletionContext
             }

initPrimitivesSqlite :: Connection -> IO ()
initPrimitivesSqlite conn = do
    let prims = map (\(i, p, b, _) -> (i, toText (messageToBuilderDB p), b)) primitives
    executeMany conn "INSERT OR REPLACE INTO Primitives (id, pattern, body) VALUES (?, ?, ?)" prims

primitivesToHaskellSqlite :: Connection -> IO ()
primitivesToHaskellSqlite conn = do
    prims <- query_ conn "SELECT id, pattern, body FROM Primitives" :: IO [(Int, T.Text, T.Text)]
    forM_ prims $ \(i, pattern, body) -> do
        putStr $ "prim" ++ show i ++ " "
        T.putStr (toText (messageToPattern (parseMessageUnsafe pattern)))
        putStr " = "
        T.putStrLn body

snapshotSqlite :: Connection -> IO Snapshot
snapshotSqlite conn = do
    workspaces <- allWorkspaces

    answers <- M.fromList . map (\(i, ma) -> (workspaceIdFromText i, parseMessageUnsafeDB ma) )
                <$> query_ conn "SELECT workspaceId, answer FROM Answers"

    answerFunctions <- query_ conn "SELECT s.sessionId, f.id \
                                   \FROM Functions f \
                                   \INNER JOIN Continuations c ON c.function = f.id \
                                   \INNER JOIN Trace t ON t.workspaceId = c.workspaceId \
                                   \INNER JOIN SessionProcesses s ON s.processId = t.processId \
                                   \WHERE f.isAnswer = 1"

    pointers <- M.fromList . map (\(p,c) -> (p, parseMessageUnsafe c)) <$> query_ conn "SELECT id, content FROM Pointers"

    alternatives <- M.fromListWith (++) . map (\(f, ps, e) -> (f, [(parsePatternsUnsafe ps, expFromDB e)]))
                        <$> query_ conn "SELECT function, pattern, body FROM Alternatives ORDER BY rowid ASC"

    links <- M.fromListWith M.union . map (\(i, s, t) -> (workspaceIdFromText i, M.singleton s t))
                <$> query_ conn "SELECT workspaceId, sourceId, targetId FROM Links"

    continuations <- allContinuations (S.fromList $ map snd answerFunctions)

    contArgs <- M.fromListWith M.union . map (\(i, f, n, v) -> ((workspaceIdFromText i, f), M.singleton n (parseMessageUnsafeDB v)))
                    <$> query_ conn "SELECT workspaceId, function, argNumber, value FROM ContinuationArguments"

    trace <- theTrace

    sessions <- M.fromListWith (++) . map (\(i, p) -> (sessionIdFromText i, [processIdFromText p]))
                    <$> query_ conn "SELECT sessionId, processId FROM SessionProcesses"

    running <- M.fromListWith (++) . map (\(i, p) -> (sessionIdFromText i, [processIdFromText p]))
                    <$> query_ conn "SELECT sessionId, processId \
                                    \FROM SessionProcesses \
                                    \WHERE processId IN (SELECT processId FROM RunQueue)"

    return $ Snapshot {
                workspacesS = workspaces,
                answersS = answers,
                answerFunctionsS = M.fromList $ map (\(sId, f) -> (sessionIdFromText sId, f)) answerFunctions,
                pointersS = pointers,
                alternativesS = alternatives,
                linksS = links,
                continuationsS = continuations,
                continuationArgumentsS = contArgs,
                traceS = trace,
                runQueueS = fmap (M.fromList . map (\p -> (p, case lookup p trace of Just s -> s))) running,
                sessionsS = sessions }
  where allWorkspaces = do
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
            let messageMap = M.fromListWith (++) $ map (\(i, m) -> (workspaceIdFromText i, [parseMessageUnsafe m])) messages
                subquestionsMap = M.fromListWith (++) $
                                    map (\(i, qId, q, ma) ->
                                            (workspaceIdFromText i,
                                             [(workspaceIdFromText qId, parseMessageUnsafe q, fmap parseMessageUnsafeDB ma)]))
                                        subquestions
                expandedMap = M.fromListWith M.union $ map (\(i, p, m) -> (workspaceIdFromText i, M.singleton p (parseMessageUnsafe' p m))) expanded
            return $ M.fromList $ map (\(i, p, t, q) -> let !wsId = workspaceIdFromText i
                                            in (wsId, Workspace {
                                                        identity = wsId,
                                                        parentId = workspaceIdFromText <$> p,
                                                        question = parseMessageUnsafeDB q,
                                                        subQuestions = maybe [] id $ M.lookup wsId subquestionsMap,
                                                        messageHistory = maybe [] id $ M.lookup wsId messageMap,
                                                        expandedPointers = maybe M.empty id $ M.lookup wsId expandedMap,
                                                        time = Time t })) workspaces

        theTrace = do
            ts <- query_ conn "SELECT processId, varEnv, funEnv, workspaceId, expression, continuation FROM Trace ORDER BY t DESC"
            return $ map (\(pId, varEnv, funEnv, s, e, k) ->
                            (processIdFromText pId,
                             (parseUnsafe parseVarEnv varEnv,
                              parseUnsafe parseFunEnv funEnv,
                              workspaceIdFromText s,
                              expFromDB e,
                              parseKont1UnsafeDB k))) ts

        allContinuations answerFunctions = do
            let toName fId | fId `S.member` answerFunctions = ANSWER
                           | otherwise = LOCAL fId
            ks <- query_ conn "SELECT workspaceId, function, next FROM Continuations"
            vars <- query_ conn "SELECT workspaceId, function, variable, value FROM ContinuationEnvironments"
            let !funEnvs = M.fromListWith (M.unionWith M.union) $
                            map (\(i, f, x, v) ->
                                    (workspaceIdFromText i, M.singleton (toName f) (M.singleton x (parseMessageUnsafeDB v)))) vars
            return $ M.fromListWith M.union $
                        map (\(i, f, k) -> let !wsId = workspaceIdFromText i
                                               !funEnv = maybe M.empty id $ M.lookup wsId funEnvs
                                           in (wsId, M.singleton f (CallKont funEnv (toName f) wsId (parseKont1UnsafeDB k)))) ks

initDBSqlite :: Connection -> IO ()
initDBSqlite conn = do
    execute_ conn "PRAGMA journal_mode = WAL;" -- Improves speed significantly when writing to a file.
    execute_ conn "PRAGMA synchronous = OFF;" -- Evil, but makes it even faster and should be fine enough for testing.
    execute_ conn "\
       \CREATE TABLE IF NOT EXISTS Workspaces (\n\
       \    id TEXT PRIMARY KEY ASC,\n\
       \    logicalTime INTEGER NOT NULL,\n\
       \    parentWorkspaceId TEXT NULL,\n\
       \    questionAsAsked TEXT NOT NULL,\n\
       \    questionAsAnswered TEXT NOT NULL,\n\
       \    FOREIGN KEY ( parentWorkspaceId ) REFERENCES Workspaces ( id ) ON DELETE CASCADE\n\
       \);"
    execute_ conn "CREATE INDEX IF NOT EXISTS Workspaces_IDX_ParentWorkspaces_Id ON Workspaces(parentWorkspaceId, id);"
    execute_ conn "\
       \CREATE TABLE IF NOT EXISTS Messages (\n\
       \    id INTEGER PRIMARY KEY ASC,\n\
       \    logicalTimeSent INTEGER NOT NULL,\n\
       \    sourceWorkspaceId TEXT NOT NULL,\n\
       \    targetWorkspaceId TEXT NOT NULL,\n\
       \    content TEXT NOT NULL,\n\
       \    FOREIGN KEY ( sourceWorkspaceId ) REFERENCES Workspaces ( id ) ON DELETE CASCADE\n\
       \    FOREIGN KEY ( targetWorkspaceId ) REFERENCES Workspaces ( id ) ON DELETE CASCADE\n\
       \);"
    execute_ conn "CREATE INDEX IF NOT EXISTS Messages_IDX_TargetWorkspaceId ON Messages(targetWorkspaceId);"
    execute_ conn "\
       \CREATE TABLE IF NOT EXISTS Pointers (\n\
       \    id INTEGER PRIMARY KEY ASC,\n\
       \    content TEXT NOT NULL\n\
       \);"
    execute_ conn "\
       \CREATE TABLE IF NOT EXISTS Answers (\n\
       \    workspaceId TEXT PRIMARY KEY ASC, -- NOT NULL,\n\
       \    logicalTimeAnswered INTEGER NOT NULL,\n\
       \    answer TEXT NOT NULL,\n\
       \    FOREIGN KEY ( workspaceId ) REFERENCES Workspaces ( id ) ON DELETE CASCADE\n\
       \);"
    execute_ conn "\
       \CREATE TABLE IF NOT EXISTS ExpandedPointers (\n\
       \    workspaceId TEXT NOT NULL,\n\
       \    pointerId INTEGER NOT NULL,\n\
       \    logicalTimeExpanded INTEGER NOT NULL,\n\
       \    FOREIGN KEY ( workspaceId ) REFERENCES Workspaces ( id ) ON DELETE CASCADE\n\
       \    FOREIGN KEY ( pointerId ) REFERENCES Pointers ( id ) ON DELETE CASCADE\n\
       \    PRIMARY KEY ( workspaceId ASC, pointerId ASC )\n\
       \);"
    execute_ conn "\
       \CREATE TABLE IF NOT EXISTS Commands (\n\
       \    workspaceId TEXT NOT NULL,\n\
       \    commandTime INTEGER NOT NULL,\n\
       \    userId TEXT NOT NULL,\n\
       \    command TEXT NOT NULL,\n\
       \    FOREIGN KEY ( workspaceId ) REFERENCES Workspaces ( id ) ON DELETE CASCADE\n\
       \    PRIMARY KEY ( workspaceId ASC, commandTime ASC )\n\
       \);"
    execute_ conn "\
       \CREATE TABLE IF NOT EXISTS Functions (\n\
       \    id INTEGER PRIMARY KEY ASC,\n\
       \    isAnswer INTEGER NOT NULL DEFAULT 0\n\
       \);"
    execute_ conn "\
       \CREATE TABLE IF NOT EXISTS Alternatives (\n\
       \    function INTEGER NOT NULL,\n\
       \    pattern TEXT NOT NULL,\n\
       \    body TEXT NOT NULL,\n\
       \    FOREIGN KEY ( function ) REFERENCES Functions ( id ) ON DELETE CASCADE\n\
       \    PRIMARY KEY ( function ASC, pattern ASC )\n\
       \);"
    execute_ conn "\
       \CREATE TABLE IF NOT EXISTS Links (\n\
       \    workspaceId TEXT NOT NULL,\n\
       \    sourceId INTEGER NOT NULL,\n\
       \    targetId INTEGER NOT NULL,\n\
       \    FOREIGN KEY ( workspaceId ) REFERENCES Workspaces ( id ) ON DELETE CASCADE\n\
       \    FOREIGN KEY ( sourceId ) REFERENCES Pointers ( id ) ON DELETE CASCADE\n\
       \    FOREIGN KEY ( targetId ) REFERENCES Pointers ( id ) ON DELETE CASCADE\n\
       \    PRIMARY KEY ( workspaceId ASC, sourceId ASC )\n\
       \);"
    execute_ conn "\
       \CREATE TABLE IF NOT EXISTS Continuations (\n\
       \    workspaceId TEXT NOT NULL,\n\
       \    function INTEGER NOT NULL,\n\
       \    next TEXT NOT NULL,\n\
       \    FOREIGN KEY ( function ) REFERENCES Functions ( id ) ON DELETE CASCADE\n\
       \    PRIMARY KEY ( workspaceId ASC, function ASC )\n\
       \);"
    execute_ conn "\
       \CREATE TABLE IF NOT EXISTS ContinuationEnvironments (\n\
       \    workspaceId TEXT NOT NULL,\n\
       \    function INTEGER NOT NULL,\n\
       \    variable INTEGER NOT NULL,\n\
       \    value TEXT NOT NULL,\n\
       \    FOREIGN KEY ( function ) REFERENCES Functions ( id ) ON DELETE CASCADE\n\
       \    FOREIGN KEY ( workspaceId ) REFERENCES Workspaces ( id ) ON DELETE CASCADE\n\
       \    FOREIGN KEY ( workspaceId, function ) REFERENCES Continuations ( workspaceId, function ) ON DELETE CASCADE\n\
       \    PRIMARY KEY ( workspaceId ASC, function ASC, variable ASC )\n\
       \);"
    execute_ conn "\
       \CREATE TABLE IF NOT EXISTS ContinuationArguments (\n\
       \    workspaceId TEXT NOT NULL,\n\
       \    function INTEGER NOT NULL,\n\
       \    argNumber INTEGER NOT NULL,\n\
       \    value TEXT NOT NULL,\n\
       \    FOREIGN KEY ( function ) REFERENCES Functions ( id ) ON DELETE CASCADE\n\
       \    FOREIGN KEY ( workspaceId ) REFERENCES Workspaces ( id ) ON DELETE CASCADE\n\
       \    FOREIGN KEY ( workspaceId, function ) REFERENCES Continuations ( workspaceId, function ) ON DELETE CASCADE\n\
       \    PRIMARY KEY ( workspaceId ASC, function ASC, argNumber ASC )\n\
       \);"
    execute_ conn "\
       \CREATE TABLE IF NOT EXISTS Trace (\n\
       \    t INTEGER PRIMARY KEY ASC,\n\
       \    processId TEXT NOT NULL,\n\
       \    varEnv TEXT NOT NULL,\n\
       \    funEnv TEXT NOT NULL,\n\
       \    workspaceId TEXT NOT NULL,\n\
       \    expression TEXT NOT NULL,\n\
       \    continuation TEXT NOT NULL,\n\
       \    FOREIGN KEY ( workspaceId ) REFERENCES Workspaces ( id ) ON DELETE CASCADE\n\
       \);"
    execute_ conn "\
       \CREATE TABLE IF NOT EXISTS RunQueue (\n\
       \    processId TEXT PRIMARY KEY\n\
       \);"
    execute_ conn "\
       \CREATE TABLE IF NOT EXISTS Sessions (\n\
       \    sessionId TEXT PRIMARY KEY ASC\n\
       \);"
    execute_ conn "\
       \CREATE TABLE IF NOT EXISTS SessionProcesses (\n\
       \    sessionId TEXT NOT NULL,\n\
       \    processId TEXT NOT NULL,\n\
       \    FOREIGN KEY ( sessionId ) REFERENCES Sessions ( sessionId ) ON DELETE CASCADE\n\
       \    PRIMARY KEY ( sessionId ASC, processId ASC )\n\
       \);"
    execute_ conn "\
       \CREATE TABLE IF NOT EXISTS Primitives (\n\
       \    id INTEGER PRIMARY KEY ASC,\n\
       \    pattern TEXT NOT NULL,\n\
       \    body TEXT NOT NULL\n\
       \);"

