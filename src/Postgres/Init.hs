{-# LANGUAGE OverloadedStrings #-}
module Postgres.Init ( makePostgresDatabaseContext ) where
import Data.Foldable ( forM_ ) -- base
import qualified Data.Map as M -- containers
import qualified Data.Set as S -- containers
import qualified Data.Text as T  -- text
import qualified Data.Text.IO as T  -- text
import Database.PostgreSQL.Simple ( Connection, Only(..), execute_, executeMany, query_ ) -- sqlite-simple

import AutoScheduler (  AutoSchedulerContext )
import Completions ( CompletionContext )
import DatabaseContext ( DatabaseContext(..), Snapshot(..) )
import Exp ( Konts(..), Name(..), parseKont1UnsafeDB, parseFunEnv, parseVarEnv, expFromDB )
import Message ( messageToBuilderDB, messageToPattern, parseMessageUnsafe, parseMessageUnsafe', parseMessageUnsafeDB,
                 parsePatternsUnsafe )
import Postgres.AutoSchedulerContext (  makePostgresAutoSchedulerContext )
import Postgres.CompletionContext ( makePostgresCompletionContext )
import Postgres.SchedulerContext ( makePostgresSchedulerContext )
import Primitive ( primitives )
import Scheduler ( SchedulerContext )
import Time ( Time(..) )
import Util ( Queue, newQueue, closeQueue, toText, parseUnsafe )
import Workspace ( Workspace(..) )

makePostgresDatabaseContext :: Connection -> IO (DatabaseContext (Connection, Queue))
makePostgresDatabaseContext conn = do
    q <- newQueue
    return $ DatabaseContext {
                initDB = do initDBPostgres conn; initPrimitivesPostgres conn,
                closeDB = closeQueue q,
                primitivesToHaskell = primitivesToHaskellPostgres conn,
                snapshot = snapshotPostgres conn,
                makeSchedulerContext = makePostgresSchedulerContext q conn,
                makeAutoSchedulerContext = makePostgresAutoSchedulerContext,
                makeCompletionContext = makePostgresCompletionContext
             }

initPrimitivesPostgres :: Connection -> IO ()
initPrimitivesPostgres conn = do
    let prims = map (\(i, p, b, _) -> (i, toText (messageToBuilderDB p), b)) primitives
    () <$ executeMany conn "INSERT INTO Primitives (id, pattern, body) VALUES (?, ?, ?) \
                           \ON CONFLICT (id) DO UPDATE SET pattern = excluded.pattern, body = excluded.body" prims

primitivesToHaskellPostgres :: Connection -> IO ()
primitivesToHaskellPostgres conn = do
    prims <- query_ conn "SELECT id, pattern, body FROM Primitives" :: IO [(Int, T.Text, T.Text)]
    forM_ prims $ \(i, pattern, body) -> do
        putStr $ "prim" ++ show i ++ " "
        T.putStr (toText (messageToPattern (parseMessageUnsafe pattern)))
        putStr " = "
        T.putStrLn body

snapshotPostgres :: Connection -> IO Snapshot
snapshotPostgres conn = do
    [Only fCounter] <- query_ conn "SELECT MAX(id) FROM Functions"

    workspaces <- allWorkspaces

    answers <- M.fromList . map (\(i, ma) -> (i, parseMessageUnsafeDB ma) )
                <$> query_ conn "SELECT workspaceId, answer FROM Answers"

    answerFunctions <- query_ conn "SELECT s.sessionId, f.id \
                                   \FROM Functions f \
                                   \INNER JOIN Continuations c ON c.function = f.id \
                                   \INNER JOIN Trace t ON t.workspaceId = c.workspaceId \
                                   \INNER JOIN SessionProcesses s ON s.processId = t.processId \
                                   \WHERE f.isAnswer = 1"

    pointers <- M.fromList . map (\(p,c) -> (p, parseMessageUnsafe c)) <$> query_ conn "SELECT id, content FROM Pointers"

    alternatives <- M.fromListWith (++) . map (\(f, ps, e) -> (f, [(parsePatternsUnsafe ps, expFromDB e)]))
                        <$> query_ conn "SELECT function, pattern, body FROM Alternatives" -- TODO: ORDER BY

    links <- M.fromListWith M.union . map (\(i, s, t) -> (i, M.singleton s t))
                <$> query_ conn "SELECT workspaceId, sourceId, targetId FROM Links"

    continuations <- allContinuations (S.fromList $ map snd answerFunctions)

    contArgs <- M.fromListWith M.union . map (\(i, f, n, v) -> ((i, f), M.singleton n (parseMessageUnsafeDB v)))
                    <$> query_ conn "SELECT workspaceId, function, argNumber, value FROM ContinuationArguments"

    trace <- theTrace

    sessions <- M.fromListWith (++) . map (\(i, p) -> (i, [p])) <$> query_ conn "SELECT sessionId, processId FROM SessionProcesses"

    running <- M.fromListWith (++) . map (\(i, p) -> (i, [p]))
                    <$> query_ conn "SELECT sessionId, processId \
                                    \FROM SessionProcesses \
                                    \WHERE processId IN (SELECT processId FROM RunQueue)"

    return $ Snapshot {
                functionCounterS = maybe 0 succ fCounter,
                workspacesS = workspaces,
                answersS = answers,
                answerFunctionsS = M.fromList $ map (\(sId, f) -> (sId, f)) answerFunctions,
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

        theTrace = do
            ts <- query_ conn "SELECT processId, varEnv, funEnv, workspaceId, expression, continuation FROM Trace ORDER BY t DESC"
            return $ map (\(pId, varEnv, funEnv, s, e, k) ->
                            (pId, (parseUnsafe parseVarEnv varEnv, parseUnsafe parseFunEnv funEnv, s, expFromDB e, parseKont1UnsafeDB k)))
                         ts

        allContinuations answerFunctions = do
            let toName fId | fId `S.member` answerFunctions = ANSWER
                           | otherwise = LOCAL fId
            ks <- query_ conn "SELECT workspaceId, function, next FROM Continuations"
            vars <- query_ conn "SELECT workspaceId, function, variable, value FROM ContinuationEnvironments"
            let !funEnvs = M.fromListWith (M.unionWith M.union) $
                            map (\(i, f, x, v) -> (i, M.singleton (toName f) (M.singleton x (parseMessageUnsafeDB v)))) vars
            return $ M.fromListWith M.union $
                        map (\(i, f, k) -> let !funEnv = maybe M.empty id $ M.lookup i funEnvs
                                           in (i, M.singleton f (CallKont funEnv (toName f) i (parseKont1UnsafeDB k)))) ks

initDBPostgres :: Connection -> IO ()
initDBPostgres conn = do
    execute_ conn "\
       \CREATE TABLE IF NOT EXISTS Workspaces (\n\
       \    id UUID PRIMARY KEY,\n\
       \    logicalTime INTEGER NOT NULL,\n\
       \    parentWorkspaceId UUID NULL,\n\
       \    questionAsAsked TEXT NOT NULL,\n\
       \    questionAsAnswered TEXT NOT NULL,\n\
       \    FOREIGN KEY ( parentWorkspaceId ) REFERENCES Workspaces ( id ) ON DELETE CASCADE\n\
       \);"
    execute_ conn "CREATE INDEX IF NOT EXISTS Workspaces_IDX_ParentWorkspaces_Id ON Workspaces(parentWorkspaceId, id);"
    execute_ conn "\
       \CREATE TABLE IF NOT EXISTS Messages (\n\
       \    id SERIAL PRIMARY KEY,\n\
       \    logicalTimeSent INTEGER NOT NULL,\n\
       \    sourceWorkspaceId UUID NOT NULL,\n\
       \    targetWorkspaceId UUID NOT NULL,\n\
       \    content TEXT NOT NULL,\n\
       \    FOREIGN KEY ( sourceWorkspaceId ) REFERENCES Workspaces ( id ) ON DELETE CASCADE,\n\
       \    FOREIGN KEY ( targetWorkspaceId ) REFERENCES Workspaces ( id ) ON DELETE CASCADE\n\
       \);"
    execute_ conn "CREATE INDEX IF NOT EXISTS Messages_IDX_TargetWorkspaceId ON Messages(targetWorkspaceId);"
    execute_ conn "\
       \CREATE TABLE IF NOT EXISTS Pointers (\n\
       \    id INTEGER PRIMARY KEY,\n\
       \    content TEXT NOT NULL\n\
       \);"
    execute_ conn "\
       \CREATE TABLE IF NOT EXISTS Answers (\n\
       \    workspaceId UUID PRIMARY KEY, -- NOT NULL,\n\
       \    logicalTimeAnswered INTEGER NOT NULL,\n\
       \    answer TEXT NOT NULL,\n\
       \    FOREIGN KEY ( workspaceId ) REFERENCES Workspaces ( id ) ON DELETE CASCADE\n\
       \);"
    execute_ conn "\
       \CREATE TABLE IF NOT EXISTS ExpandedPointers (\n\
       \    workspaceId UUID NOT NULL,\n\
       \    pointerId INTEGER NOT NULL,\n\
       \    logicalTimeExpanded INTEGER NOT NULL,\n\
       \    FOREIGN KEY ( workspaceId ) REFERENCES Workspaces ( id ) ON DELETE CASCADE,\n\
       \    FOREIGN KEY ( pointerId ) REFERENCES Pointers ( id ) ON DELETE CASCADE,\n\
       \    PRIMARY KEY ( workspaceId, pointerId )\n\
       \);"
    execute_ conn "\
       \CREATE TABLE IF NOT EXISTS Commands (\n\
       \    workspaceId UUID NOT NULL,\n\
       \    commandTime INTEGER NOT NULL,\n\
       \    userId UUID NOT NULL,\n\
       \    command TEXT NOT NULL,\n\
       \    FOREIGN KEY ( workspaceId ) REFERENCES Workspaces ( id ) ON DELETE CASCADE,\n\
       \    PRIMARY KEY ( workspaceId, commandTime )\n\
       \);"
    execute_ conn "\
       \CREATE TABLE IF NOT EXISTS Functions (\n\
       \    id SERIAL PRIMARY KEY,\n\
       \    isAnswer INTEGER NOT NULL DEFAULT 0\n\
       \);"
    execute_ conn "\
       \CREATE TABLE IF NOT EXISTS Alternatives (\n\
       \    function INTEGER NOT NULL,\n\
       \    pattern TEXT NOT NULL,\n\
       \    body TEXT NOT NULL,\n\
       \    FOREIGN KEY ( function ) REFERENCES Functions ( id ) ON DELETE CASCADE,\n\
       \    PRIMARY KEY ( function, pattern )\n\
       \);"
    execute_ conn "\
       \CREATE TABLE IF NOT EXISTS Links (\n\
       \    workspaceId UUID NOT NULL,\n\
       \    sourceId INTEGER NOT NULL,\n\
       \    targetId INTEGER NOT NULL,\n\
       \    FOREIGN KEY ( workspaceId ) REFERENCES Workspaces ( id ) ON DELETE CASCADE,\n\
       \    FOREIGN KEY ( sourceId ) REFERENCES Pointers ( id ) ON DELETE CASCADE,\n\
       \    FOREIGN KEY ( targetId ) REFERENCES Pointers ( id ) ON DELETE CASCADE,\n\
       \    PRIMARY KEY ( workspaceId, sourceId )\n\
       \);"
    execute_ conn "\
       \CREATE TABLE IF NOT EXISTS Continuations (\n\
       \    workspaceId UUID NOT NULL,\n\
       \    function INTEGER NOT NULL,\n\
       \    next TEXT NOT NULL,\n\
       \    FOREIGN KEY ( function ) REFERENCES Functions ( id ) ON DELETE CASCADE,\n\
       \    PRIMARY KEY ( workspaceId, function )\n\
       \);"
    execute_ conn "\
       \CREATE TABLE IF NOT EXISTS ContinuationEnvironments (\n\
       \    workspaceId UUID NOT NULL,\n\
       \    function INTEGER NOT NULL,\n\
       \    variable INTEGER NOT NULL,\n\
       \    value TEXT NOT NULL,\n\
       \    FOREIGN KEY ( function ) REFERENCES Functions ( id ) ON DELETE CASCADE,\n\
       \    FOREIGN KEY ( workspaceId ) REFERENCES Workspaces ( id ) ON DELETE CASCADE,\n\
       \    FOREIGN KEY ( workspaceId, function ) REFERENCES Continuations ( workspaceId, function ) ON DELETE CASCADE,\n\
       \    PRIMARY KEY ( workspaceId, function, variable )\n\
       \);"
    execute_ conn "\
       \CREATE TABLE IF NOT EXISTS ContinuationArguments (\n\
       \    workspaceId UUID NOT NULL,\n\
       \    function INTEGER NOT NULL,\n\
       \    argNumber INTEGER NOT NULL,\n\
       \    value TEXT NOT NULL,\n\
       \    FOREIGN KEY ( function ) REFERENCES Functions ( id ) ON DELETE CASCADE,\n\
       \    FOREIGN KEY ( workspaceId ) REFERENCES Workspaces ( id ) ON DELETE CASCADE,\n\
       \    FOREIGN KEY ( workspaceId, function ) REFERENCES Continuations ( workspaceId, function ) ON DELETE CASCADE,\n\
       \    PRIMARY KEY ( workspaceId, function, argNumber )\n\
       \);"
    execute_ conn "\
       \CREATE TABLE IF NOT EXISTS Trace (\n\
       \    t SERIAL PRIMARY KEY,\n\
       \    processId UUID NOT NULL,\n\
       \    varEnv TEXT NOT NULL,\n\
       \    funEnv TEXT NOT NULL,\n\
       \    workspaceId UUID NOT NULL,\n\
       \    expression TEXT NOT NULL,\n\
       \    continuation TEXT NOT NULL,\n\
       \    FOREIGN KEY ( workspaceId ) REFERENCES Workspaces ( id ) ON DELETE CASCADE\n\
       \);"
    execute_ conn "\
       \CREATE TABLE IF NOT EXISTS RunQueue (\n\
       \    processId UUID PRIMARY KEY\n\
       \);"
    execute_ conn "\
       \CREATE TABLE IF NOT EXISTS Sessions (\n\
       \    sessionId UUID PRIMARY KEY\n\
       \);"
    execute_ conn "\
       \CREATE TABLE IF NOT EXISTS SessionProcesses (\n\
       \    sessionId UUID NOT NULL,\n\
       \    processId UUID NOT NULL,\n\
       \    FOREIGN KEY ( sessionId ) REFERENCES Sessions ( sessionId ) ON DELETE CASCADE,\n\
       \    PRIMARY KEY ( sessionId, processId )\n\
       \);"
    execute_ conn "\
       \CREATE TABLE IF NOT EXISTS Primitives (\n\
       \    id INTEGER PRIMARY KEY,\n\
       \    pattern TEXT NOT NULL,\n\
       \    body TEXT NOT NULL\n\
       \);"
    return ()
