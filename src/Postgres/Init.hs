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
    () <$ executeMany conn "INSERT INTO Primitives ( id, pattern, body ) VALUES (?, ?, ?) \
                           \ON CONFLICT (id) DO UPDATE SET pattern = excluded.pattern, body = excluded.body" prims

primitivesToHaskellPostgres :: Connection -> IO ()
primitivesToHaskellPostgres conn = do
    prims <- query_ conn "SELECT id, pattern, body FROM Primitives" :: IO [(Int, T.Text, T.Text)]
    forM_ prims $ \(i, pattern, body) -> do
        putStr $ "prim" ++ show i ++ " "
        T.putStr (toText (messageToPattern (parseMessageUnsafe pattern)))
        putStr " = "
        T.putStrLn body

-- TODO: Completely go over this to handle versions.
snapshotPostgres :: Connection -> IO Snapshot
snapshotPostgres conn = do
    [Only fCounter] <- query_ conn "SELECT MAX(id) FROM Functions"

    workspaces <- allWorkspaces

    answers <- M.fromList . map (\(i, ma) -> (i, parseMessageUnsafeDB ma) )
                <$> query_ conn "SELECT versionId, answer FROM Answers"

    answerFunctions <- query_ conn "SELECT s.sessionId, f.id \
                                   \FROM Functions f \
                                   \INNER JOIN Continuations c ON c.function = f.id \
                                   \INNER JOIN Trace t ON t.versionId = c.versionId \
                                   \INNER JOIN SessionProcesses s ON s.processId = t.processId \
                                   \WHERE f.isAnswer = 1"

    pointers <- M.fromList . map (\(p,c) -> (p, parseMessageUnsafe c)) <$> query_ conn "SELECT id, content FROM Pointers"

    alternatives <- M.fromListWith (++) . map (\(f, ps, e) -> (f, [(parsePatternsUnsafe ps, expFromDB e)]))
                        <$> query_ conn "SELECT function, pattern, body FROM Alternatives" -- TODO: ORDER BY

    links <- M.fromListWith M.union . map (\(i, s, t) -> (i, M.singleton s t))
                <$> query_ conn "SELECT versionId, sourceId, targetId FROM Links"

    continuations <- allContinuations (S.fromList $ map snd answerFunctions)

    contArgs <- M.fromListWith M.union . map (\(i, f, n, v) -> ((i, f), M.singleton n (parseMessageUnsafeDB v)))
                    <$> query_ conn "SELECT versionId, function, argNumber, value FROM ContinuationArguments"

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
            workspaces <- query_ conn "SELECT versionId, id, parentWorkspaceVersionId, previousVersion, logicalTime, questionAsAnswered \
                                      \FROM Workspaces"
            messages <- query_ conn "SELECT targetWorkspaceVersionId, content FROM Messages" -- TODO: ORDER
            subquestions <- query_ conn "SELECT p.id, q.id, q.questionAsAsked, a.answer \
                                        \FROM Workspaces p \
                                        \INNER JOIN Workspaces q ON q.parentWorkspaceVersionId = p.id -- FIXME \
                                        \LEFT OUTER JOIN Answers a ON q.id = a.versionId \
                                        \ORDER BY p.id ASC, q.logicalTime DESC"
            expanded <- query_ conn "SELECT versionId, pointerId, content \
                                    \FROM ExpandedPointers e \
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

        theTrace = do
            ts <- query_ conn "SELECT processId, varEnv, funEnv, versionId, expression, continuation FROM Trace ORDER BY t DESC"
            return $ map (\(pId, varEnv, funEnv, s, e, k) ->
                            (pId, (parseUnsafe parseVarEnv varEnv, parseUnsafe parseFunEnv funEnv, s, expFromDB e, parseKont1UnsafeDB k)))
                         ts

        allContinuations answerFunctions = do
            let toName fId | fId `S.member` answerFunctions = ANSWER
                           | otherwise = LOCAL fId
            ks <- query_ conn "SELECT versionId, function, next FROM Continuations"
            vars <- query_ conn "SELECT versionId, function, variable, value FROM ContinuationEnvironments"
            let !funEnvs = M.fromListWith (M.unionWith M.union) $
                            map (\(i, f, x, v) -> (i, M.singleton (toName f) (M.singleton x (parseMessageUnsafeDB v)))) vars
            return $ M.fromListWith M.union $
                        map (\(i, f, k) -> let !funEnv = maybe M.empty id $ M.lookup i funEnvs
                                           in (i, M.singleton f (CallKont funEnv (toName f) i (parseKont1UnsafeDB k)))) ks

-- TODO: Figure out what indexes need to be added and add them.
initDBPostgres :: Connection -> IO ()
initDBPostgres conn = do
    execute_ conn "\
        \CREATE TABLE IF NOT EXISTS Workspaces (\n\
        \   id UUID PRIMARY KEY,\n\
        \   questionAsAsked TEXT NOT NULL,\n\
        \   questionAsAnswered TEXT NOT NULL\n\
        \);"
    execute_ conn "\
        \CREATE TABLE IF NOT EXISTS WorkspaceVersions (\n\
        \   versionId UUID PRIMARY KEY,\n\
        \   workspaceId UUID NOT NULL,\n\
        \   parentWorkspaceVersionId UUID NULL,\n\
        \   logicalTime INTEGER NOT NULL,\n\
        \   previousVersion UUID NULL,\n\
        \   FOREIGN KEY ( workspaceId ) REFERENCES Workspaces ( id ) ON DELETE CASCADE,\n\
        \   FOREIGN KEY ( parentWorkspaceVersionId ) REFERENCES WorkspaceVersions ( versionId ) ON DELETE CASCADE,\n\
        \   FOREIGN KEY ( previousVersion ) REFERENCES WorkspaceVersions ( versionId ) ON DELETE CASCADE\n\
        \);"
    execute_ conn "\
        \CREATE TABLE IF NOT EXISTS Messages (\n\
        \   id SERIAL PRIMARY KEY,\n\
        \   sourceWorkspaceVersionId UUID NOT NULL,\n\
        \   targetWorkspaceVersionId UUID NOT NULL,\n\
        \   content TEXT NOT NULL,\n\
        \   FOREIGN KEY ( sourceWorkspaceVersionId ) REFERENCES WorkspaceVersions ( versionId ) ON DELETE CASCADE,\n\
        \   FOREIGN KEY ( targetWorkspaceVersionId ) REFERENCES WorkspaceVersions ( versionId ) ON DELETE CASCADE\n\
        \);"
    execute_ conn "CREATE INDEX IF NOT EXISTS Messages_IDX_TargetWorkspaceId ON Messages(targetWorkspaceVersionId);"
    execute_ conn "\
        \CREATE TABLE IF NOT EXISTS Pointers (\n\
        \   id INTEGER PRIMARY KEY,\n\
        \   content TEXT NOT NULL\n\
        \);"
    execute_ conn "\
        \CREATE TABLE IF NOT EXISTS Answers (\n\
        \   versionId UUID PRIMARY KEY,\n\
        \   answer TEXT NOT NULL,\n\
        \   FOREIGN KEY ( versionId ) REFERENCES WorkspaceVersions ( versionId ) ON DELETE CASCADE\n\
        \);"
    execute_ conn "\
        \CREATE TABLE IF NOT EXISTS ExpandedPointers (\n\
        \   versionId UUID NOT NULL,\n\
        \   pointerId INTEGER NOT NULL,\n\
        \   FOREIGN KEY ( versionId ) REFERENCES WorkspaceVersions ( versionId ) ON DELETE CASCADE,\n\
        \   FOREIGN KEY ( pointerId ) REFERENCES Pointers ( id ) ON DELETE CASCADE,\n\
        \   PRIMARY KEY ( versionId, pointerId )\n\
        \);"
    execute_ conn "\
        \CREATE TABLE IF NOT EXISTS Commands (\n\
        \   versionId UUID NOT NULL,\n\
        \   commandTime INTEGER NOT NULL,\n\
        \   userId UUID NOT NULL,\n\
        \   command TEXT NOT NULL,\n\
        \   FOREIGN KEY ( versionId ) REFERENCES WorkspaceVersions ( versionId ) ON DELETE CASCADE,\n\
        \   PRIMARY KEY ( versionId, commandTime )\n\
        \);"
    execute_ conn "\
        \CREATE TABLE IF NOT EXISTS Functions (\n\
        \   id SERIAL PRIMARY KEY,\n\
        \   isAnswer INTEGER NOT NULL DEFAULT 0\n\
        \);"
    execute_ conn "\
        \CREATE TABLE IF NOT EXISTS Alternatives (\n\
        \   function INTEGER NOT NULL,\n\
        \   pattern TEXT NOT NULL,\n\
        \   body TEXT NOT NULL,\n\
        \   FOREIGN KEY ( function ) REFERENCES Functions ( id ) ON DELETE CASCADE,\n\
        \   PRIMARY KEY ( function, pattern )\n\
        \);"
    execute_ conn "\
        \CREATE TABLE IF NOT EXISTS Links (\n\
        \   versionId UUID NOT NULL,\n\
        \   sourceId INTEGER NOT NULL,\n\
        \   targetId INTEGER NOT NULL,\n\
        \   FOREIGN KEY ( versionId ) REFERENCES WorkspaceVersions ( versionId ) ON DELETE CASCADE,\n\
        \   FOREIGN KEY ( sourceId ) REFERENCES Pointers ( id ) ON DELETE CASCADE,\n\
        \   FOREIGN KEY ( targetId ) REFERENCES Pointers ( id ) ON DELETE CASCADE,\n\
        \   PRIMARY KEY ( versionId, sourceId )\n\
        \);"
    execute_ conn "\
        \CREATE TABLE IF NOT EXISTS Continuations (\n\
        \   versionId UUID NOT NULL,\n\
        \   function INTEGER NOT NULL,\n\
        \   next TEXT NOT NULL,\n\
        \   FOREIGN KEY ( versionId ) REFERENCES WorkspaceVersions ( versionId ) ON DELETE CASCADE,\n\
        \   FOREIGN KEY ( function ) REFERENCES Functions ( id ) ON DELETE CASCADE,\n\
        \   PRIMARY KEY ( versionId, function )\n\
        \);"
    execute_ conn "\
        \CREATE TABLE IF NOT EXISTS ContinuationEnvironments (\n\
        \   versionId UUID NOT NULL,\n\
        \   function INTEGER NOT NULL,\n\
        \   variable INTEGER NOT NULL,\n\
        \   value TEXT NOT NULL,\n\
        \   FOREIGN KEY ( function ) REFERENCES Functions ( id ) ON DELETE CASCADE,\n\
        \   FOREIGN KEY ( versionId ) REFERENCES WorkspaceVersions ( versionId ) ON DELETE CASCADE,\n\
        \   FOREIGN KEY ( versionId, function ) REFERENCES Continuations ( versionId, function ) ON DELETE CASCADE,\n\
        \   PRIMARY KEY ( versionId, function, variable )\n\
        \);"
    execute_ conn "\
        \CREATE TABLE IF NOT EXISTS ContinuationArguments (\n\
        \   versionId UUID NOT NULL,\n\
        \   function INTEGER NOT NULL,\n\
        \   argNumber INTEGER NOT NULL,\n\
        \   value TEXT NOT NULL,\n\
        \   FOREIGN KEY ( function ) REFERENCES Functions ( id ) ON DELETE CASCADE,\n\
        \   FOREIGN KEY ( versionId ) REFERENCES WorkspaceVersions ( versionId ) ON DELETE CASCADE,\n\
        \   FOREIGN KEY ( versionId, function ) REFERENCES Continuations ( versionId, function ) ON DELETE CASCADE,\n\
        \   PRIMARY KEY ( versionId, function, argNumber )\n\
        \);"
    execute_ conn "\
        \CREATE TABLE IF NOT EXISTS Trace (\n\
        \   t SERIAL PRIMARY KEY,\n\
        \   processId UUID NOT NULL,\n\
        \   varEnv TEXT NOT NULL,\n\
        \   funEnv TEXT NOT NULL,\n\
        \   versionId UUID NOT NULL,\n\
        \   expression TEXT NOT NULL,\n\
        \   continuation TEXT NOT NULL,\n\
        \   FOREIGN KEY ( versionId ) REFERENCES WorkspaceVersions ( versionId ) ON DELETE CASCADE\n\
        \);"
    execute_ conn "\
        \CREATE TABLE IF NOT EXISTS RunQueue (\n\
        \   processId UUID PRIMARY KEY\n\
        \);"
    execute_ conn "\
        \CREATE TABLE IF NOT EXISTS Sessions (\n\
        \   sessionId UUID PRIMARY KEY\n\
        \);"
    execute_ conn "\
        \CREATE TABLE IF NOT EXISTS SessionProcesses (\n\
        \   sessionId UUID NOT NULL,\n\
        \   processId UUID NOT NULL,\n\
        \   FOREIGN KEY ( sessionId ) REFERENCES Sessions ( sessionId ) ON DELETE CASCADE,\n\
        \   PRIMARY KEY ( sessionId, processId )\n\
        \);"
    execute_ conn "\
        \CREATE TABLE IF NOT EXISTS Primitives (\n\
        \   id INTEGER PRIMARY KEY,\n\
        \   pattern TEXT NOT NULL,\n\
        \   body TEXT NOT NULL\n\
        \);"
    execute_ conn "\
        \CREATE OR REPLACE FUNCTION priorVersions(workspaceVersionId UUID)\n\
        \RETURNS TABLE (versionId UUID) LANGUAGE SQL STABLE ROWS 20 AS $$\n\
        \   WITH RECURSIVE versions(id) AS (\n\
        \       SELECT workspaceVersionId\n\
        \       UNION ALL\n\
        \       SELECT w.previousVersion\n\
        \       FROM WorkspaceVersions w\n\
        \       INNER JOIN versions v ON w.versionId = v.id\n\
        \       WHERE w.previousVersion IS NOT NULL\n\
        \   )\n\
        \   SELECT id AS versionId FROM versions\n\
        \$$"
    execute_ conn "\
        \CREATE OR REPLACE FUNCTION messagesAsOf(pVersionId UUID)\n\
        \RETURNS TABLE (id INTEGER, sourceWorkspaceVersionId UUID, targetWorkspaceVersionId UUID, content TEXT) LANGUAGE SQL STABLE AS $$\n\
        \   SELECT m.id, pVersionId AS sourceWorkspaceVersionId, m.targetWorkspaceVersionId, m.content\n\
        \   FROM priorVersions(pVersionId) w\n\
        \   INNER JOIN Messages m ON m.sourceWorkspaceVersionId = w.versionId\n\
        \   INNER JOIN WorkspaceVersions t ON t.versionId = m.targetWorkspaceVersionId\n\
        \$$"
    execute_ conn "\
        \CREATE OR REPLACE FUNCTION expandedPointersAsOf(pVersionId UUID)\n\
        \RETURNS TABLE (versionId UUID, pointerId INTEGER) LANGUAGE SQL STABLE AS $$\n\
        \   SELECT pVersionId, p.pointerId\n\
        \   FROM priorVersions(pVersionId) w\n\
        \   INNER JOIN ExpandedPointers p ON p.versionId = w.versionId\n\
        \$$"
    execute_ conn "\
        \CREATE OR REPLACE FUNCTION subquestionsAsOf(pVersionId UUID)\n\
        \RETURNS TABLE (parentWorkspaceVersionId UUID, workspaceId UUID, versionId UUID, logicalTime INTEGER, questionAsAsked TEXT, answer TEXT)\n\
        \LANGUAGE SQL STABLE AS $$\n\
        \   SELECT pVersionId AS parentWorkspaceVersionId, c.workspaceId, c.versionId,\n\
        \          c.logicalTime, cw.questionAsAsked, a.answer\n\
        \   FROM priorVersions(pVersionId) w\n\
        \   INNER JOIN WorkspaceVersions c ON c.parentWorkspaceVersionId = w.versionId\n\
        \   INNER JOIN Workspaces cw ON cw.id = c.workspaceId\n\
        \   LEFT OUTER JOIN Answers a ON a.versionId = c.versionId\n\
        \$$"
    -- TODO: Use a materialized view? Maybe a materialized view of the latest workspace and all its previous versions.
    -- The workspace versions that aren't the previous version of any other workspace version are the leaves of the
    -- branching timeline. Then, the leaf with the latest logicalTime will be the version on the "active" timeline
    -- assuming the workspace exists at all on the "active" timeline.
    execute_ conn "\
        \CREATE OR REPLACE VIEW Latest_WorkspaceVersions AS\n\
        \   SELECT DISTINCT\n\
        \       c.workspaceId,\n\
        \       FIRST_VALUE(c.versionId) OVER w AS versionId,\n\
        \       FIRST_VALUE(c.parentWorkspaceVersionId) OVER w AS parentWorkspaceVersionId,\n\
        \       FIRST_VALUE(c.logicalTime) OVER w AS logicalTime\n\
        \   FROM WorkspaceVersions c\n\
        \   WINDOW w AS (PARTITION BY c.workspaceId ORDER BY c.logicalTime DESC)"
    -- The Current_* views are aggregating all prior versions of a latest workspace version, e.g. all pointers that were
    -- expanded in any prior version of a latest workspace version.
    -- TODO: Need this one?
    execute_ conn "\
        \CREATE OR REPLACE VIEW Current_Links AS\n\
        \   SELECT l.workspaceId, l.versionId, j.sourceId, j.targetId\n\
        \   FROM Latest_WorkspaceVersions l\n\
        \   CROSS JOIN priorVersions(l.versionId) w\n\
        \   INNER JOIN Links j ON j.versionId = w.versionId"
    execute_ conn "\
        \CREATE OR REPLACE VIEW Current_Messages AS\n\
        \   SELECT m.id, l.workspaceId AS sourceWorkspaceId, l.versionId AS sourceWorkspaceVersionId,\n\
        \                t.workspaceId AS targetWorkspaceId, m.targetWorkspaceVersionId, m.content\n\
        \   FROM Latest_WorkspaceVersions l\n\
        \   CROSS JOIN priorVersions(l.versionId) w\n\
        \   INNER JOIN Messages m ON m.sourceWorkspaceVersionId = w.versionId\n\
        \   INNER JOIN WorkspaceVersions t ON t.versionId = m.targetWorkspaceVersionId"
    execute_ conn "\
        \CREATE OR REPLACE VIEW Current_ExpandedPointers AS\n\
        \   SELECT l.workspaceId, l.versionId, p.pointerId\n\
        \   FROM Latest_WorkspaceVersions l\n\
        \   CROSS JOIN priorVersions(l.versionId) w\n\
        \   INNER JOIN ExpandedPointers p ON p.versionId = w.versionId"
    execute_ conn "\
        \CREATE OR REPLACE VIEW Current_Subquestions AS\n\
        \   SELECT l.workspaceId AS parentWorkspaceId, l.versionId AS parentWorkspaceVersionId, c.workspaceId, c.versionId,\n\
        \          c.logicalTime, cw.questionAsAsked, a.answer\n\
        \   FROM Latest_WorkspaceVersions l\n\
        \   CROSS JOIN priorVersions(l.versionId) w\n\
        \   INNER JOIN Latest_WorkspaceVersions c ON c.parentWorkspaceVersionId = w.versionId\n\
        \   INNER JOIN Workspaces cw ON cw.id = c.workspaceId\n\
        \   LEFT OUTER JOIN Answers a ON a.versionId = c.versionId"
    return ()
