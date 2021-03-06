{-# LANGUAGE OverloadedStrings #-}
module Sqlite.Init ( makeSqliteDatabaseContext ) where
import Data.Foldable ( forM_ ) -- base
import qualified Data.Map as M -- containers
import qualified Data.Set as S -- containers
import qualified Data.Text as T  -- text
import qualified Data.Text.IO as T  -- text
import Database.SQLite.Simple ( Connection, Only(..), execute_, executeMany, query_ ) -- sqlite-simple

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
import Workspace ( Workspace(..), versionIdFromText, workspaceIdFromText )

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
    [Only fCounter] <- query_ conn "SELECT MAX(id) FROM Functions"

    workspaces <- allWorkspaces

    answers <- M.fromList . map (\(i, ma) -> (versionIdFromText i, parseMessageUnsafeDB ma) )
                <$> query_ conn "SELECT versionId, answer FROM Answers"

    answerFunctions <- query_ conn "SELECT s.sessionId, f.id \
                                   \FROM Functions f \
                                   \INNER JOIN Continuations c ON c.function = f.id \
                                   \INNER JOIN Trace t ON t.versionId = c.versionId \
                                   \INNER JOIN SessionProcesses s ON s.processId = t.processId \
                                   \WHERE f.isAnswer = 1"

    pointers <- M.fromList . map (\(p,c) -> (p, parseMessageUnsafe c)) <$> query_ conn "SELECT id, content FROM Pointers"

    alternatives <- M.fromListWith (++) . map (\(f, ps, e) -> (f, [(parsePatternsUnsafe ps, expFromDB e)]))
                        <$> query_ conn "SELECT function, pattern, body FROM Alternatives ORDER BY rowid ASC"

    links <- M.fromListWith M.union . map (\(i, s, t) -> (versionIdFromText i, M.singleton s t))
                <$> query_ conn "SELECT versionId, sourceId, targetId FROM Links"

    continuations <- allContinuations (S.fromList $ map snd answerFunctions)

    contArgs <- M.fromListWith M.union . map (\(i, f, n, v) -> ((versionIdFromText i, f), M.singleton n (parseMessageUnsafeDB v)))
                    <$> query_ conn "SELECT versionId, function, argNumber, value FROM ContinuationArguments"

    trace <- theTrace

    sessions <- M.fromListWith (++) . map (\(i, p) -> (sessionIdFromText i, [processIdFromText p]))
                    <$> query_ conn "SELECT sessionId, processId FROM SessionProcesses"

    running <- M.fromListWith (++) . map (\(i, p) -> (sessionIdFromText i, [processIdFromText p]))
                    <$> query_ conn "SELECT sessionId, processId \
                                    \FROM SessionProcesses \
                                    \WHERE processId IN (SELECT processId FROM RunQueue)"

    return $ Snapshot {
                functionCounterS = maybe 0 succ fCounter,
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
            workspaces <- query_ conn "SELECT v.versionId, v.workspaceId, v.parentWorkspaceVersionId, v.previousVersion, v.logicalTime, w.questionAsAnswered \
                                      \FROM WorkspaceVersions v \
                                      \INNER JOIN Workspaces w ON w.id = v.workspaceId"
            messages <- query_ conn "SELECT d.versionId, m.content \
                                    \FROM WorkspaceVersions d \
                                    \INNER JOIN WorkspaceVersions tgt ON tgt.workspaceId = d.workspaceId AND tgt.logicalTime <= d.logicalTime \
                                    \INNER JOIN Messages m ON m.targetWorkspaceVersionId = tgt.versionId \
                                    \ORDER BY tgt.logicalTime ASC"
            subquestions <- query_ conn "SELECT d.versionId, q.versionId, qw.questionAsAsked, a.answer \
                                        \FROM WorkspaceVersions d \
                                        \INNER JOIN WorkspaceVersions p ON p.workspaceId = d.workspaceId AND p.logicalTime <= d.logicalTime \
                                        \INNER JOIN ( \
                                        \   SELECT q.*, RANK() OVER (PARTITION BY q.workspaceId ORDER BY q.logicalTime DESC) AS ranking \
                                        \   FROM WorkspaceVersions q) q ON q.parentWorkspaceVersionId = p.versionId AND q.ranking = 1 \
                                        \INNER JOIN Workspaces qw ON qw.id = q.workspaceId \
                                        \LEFT OUTER JOIN Answers a ON q.versionId = a.versionId \
                                        \ORDER BY p.versionId ASC, q.logicalTime DESC"
            expanded <- query_ conn "SELECT d.versionId, e.pointerId, p.content \
                                    \FROM WorkspaceVersions d \
                                    \INNER JOIN WorkspaceVersions v ON v.workspaceId = d.workspaceId AND v.logicalTime <= d.logicalTime \
                                    \INNER JOIN ExpandedPointers e ON e.versionId = v.versionId \
                                    \INNER JOIN Pointers p ON e.pointerId = p.id"

            let messageMap = M.fromListWith (++) $ map (\(i, m) -> (versionIdFromText i, [parseMessageUnsafe m])) messages
                subquestionsMap = M.fromListWith (++) $
                                    map (\(i, qId, q, ma) ->
                                            (versionIdFromText i,
                                             [(versionIdFromText qId, parseMessageUnsafe q, fmap parseMessageUnsafeDB ma)]))
                                        subquestions
                expandedMap = M.fromListWith M.union $ map (\(i, p, m) -> (versionIdFromText i, M.singleton p (parseMessageUnsafe' p m))) expanded
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

        theTrace = do
            ts <- query_ conn "SELECT processId, varEnv, funEnv, versionId, expression, continuation FROM Trace ORDER BY t DESC"
            return $ map (\(pId, varEnv, funEnv, s, e, k) ->
                            (processIdFromText pId,
                             (parseUnsafe parseVarEnv varEnv,
                              parseUnsafe parseFunEnv funEnv,
                              versionIdFromText s,
                              expFromDB e,
                              parseKont1UnsafeDB k))) ts

        allContinuations answerFunctions = do
            let toName fId | fId `S.member` answerFunctions = ANSWER
                           | otherwise = LOCAL fId
            ks <- query_ conn "SELECT versionId, function, next FROM Continuations"
            vars <- query_ conn "SELECT versionId, function, variable, value FROM ContinuationEnvironments"
            let !funEnvs = M.fromListWith (M.unionWith M.union) $
                            map (\(i, f, x, v) ->
                                    (versionIdFromText i, M.singleton (toName f) (M.singleton x (parseMessageUnsafeDB v)))) vars
            return $ M.fromListWith M.union $
                        map (\(i, f, k) -> let !wsId = versionIdFromText i
                                               !funEnv = maybe M.empty id $ M.lookup wsId funEnvs
                                           in (wsId, M.singleton f (CallKont funEnv (toName f) wsId (parseKont1UnsafeDB k)))) ks

initDBSqlite :: Connection -> IO ()
initDBSqlite conn = do
    execute_ conn "PRAGMA journal_mode = WAL;" -- Improves speed significantly when writing to a file.
    execute_ conn "PRAGMA synchronous = OFF;" -- Evil, but makes it even faster and should be fine enough for testing.
    execute_ conn "\
        \CREATE TABLE IF NOT EXISTS Workspaces (\n\
        \   id TEXT PRIMARY KEY,\n\
        \   questionAsAsked TEXT NOT NULL,\n\
        \   questionAsAnswered TEXT NOT NULL\n\
        \);"
    execute_ conn "\
        \CREATE TABLE IF NOT EXISTS WorkspaceVersions (\n\
        \   versionId TEXT PRIMARY KEY,\n\
        \   workspaceId TEXT NOT NULL,\n\
        \   parentWorkspaceVersionId TEXT NULL,\n\
        \   logicalTime INTEGER NOT NULL,\n\
        \   previousVersion TEXT NULL,\n\
        \   FOREIGN KEY ( workspaceId ) REFERENCES Workspaces ( id ) ON DELETE CASCADE,\n\
        \   FOREIGN KEY ( parentWorkspaceVersionId ) REFERENCES WorkspaceVersions ( versionId ) ON DELETE CASCADE,\n\
        \   FOREIGN KEY ( previousVersion ) REFERENCES WorkspaceVersions ( versionId ) ON DELETE CASCADE\n\
        \);"
    execute_ conn "\
        \CREATE TABLE IF NOT EXISTS Messages (\n\
        \   id PRIMARY KEY,\n\
        \   sourceWorkspaceVersionId TEXT NOT NULL,\n\
        \   targetWorkspaceVersionId TEXT NOT NULL,\n\
        \   content TEXT NOT NULL,\n\
        \   FOREIGN KEY ( sourceWorkspaceVersionId ) REFERENCES WorkspaceVersions ( versionId ) ON DELETE CASCADE,\n\
        \   FOREIGN KEY ( targetWorkspaceVersionId ) REFERENCES WorkspaceVersions ( versionId ) ON DELETE CASCADE\n\
        \);"
    execute_ conn "CREATE INDEX IF NOT EXISTS Messages_IDX_TargetWorkspaceId ON Messages(targetWorkspaceVersionId);"
    execute_ conn "\
        \CREATE TABLE IF NOT EXISTS Pointers (\n\
        \   id INTEGER PRIMARY KEY ASC,\n\
        \   content TEXT NOT NULL\n\
        \);"
    execute_ conn "\
        \CREATE TABLE IF NOT EXISTS Answers (\n\
        \   versionId TEXT PRIMARY KEY,\n\
        \   answer TEXT NOT NULL,\n\
        \   FOREIGN KEY ( versionId ) REFERENCES WorkspaceVersions ( versionId ) ON DELETE CASCADE\n\
        \);"
    execute_ conn "\
        \CREATE TABLE IF NOT EXISTS ExpandedPointers (\n\
        \   versionId TEXT NOT NULL,\n\
        \   pointerId INTEGER NOT NULL,\n\
        \   FOREIGN KEY ( versionId ) REFERENCES WorkspaceVersions ( versionId ) ON DELETE CASCADE,\n\
        \   FOREIGN KEY ( pointerId ) REFERENCES Pointers ( id ) ON DELETE CASCADE,\n\
        \   PRIMARY KEY ( versionId, pointerId )\n\
        \);"
    execute_ conn "\
        \CREATE TABLE IF NOT EXISTS Commands (\n\
        \   versionId TEXT NOT NULL,\n\
        \   commandTime INTEGER NOT NULL,\n\
        \   userId TEXT NOT NULL,\n\
        \   command TEXT NOT NULL,\n\
        \   FOREIGN KEY ( versionId ) REFERENCES WorkspaceVersions ( versionId ) ON DELETE CASCADE,\n\
        \   PRIMARY KEY ( versionId, commandTime )\n\
        \);"
    execute_ conn "\
        \CREATE TABLE IF NOT EXISTS Functions (\n\
        \   id INTEGER PRIMARY KEY ASC,\n\
        \   isAnswer INTEGER NOT NULL DEFAULT 0\n\
        \);"
    execute_ conn "\
        \CREATE TABLE IF NOT EXISTS Alternatives (\n\
        \   function INTEGER NOT NULL,\n\
        \   pattern TEXT NOT NULL,\n\
        \   body TEXT NOT NULL,\n\
        \   FOREIGN KEY ( function ) REFERENCES Functions ( id ) ON DELETE CASCADE\n\
        \   PRIMARY KEY ( function ASC, pattern ASC )\n\
        \);"
    execute_ conn "\
        \CREATE TABLE IF NOT EXISTS Links (\n\
        \   versionId TEXT NOT NULL,\n\
        \   sourceId INTEGER NOT NULL,\n\
        \   targetId INTEGER NOT NULL,\n\
        \   FOREIGN KEY ( versionId ) REFERENCES WorkspaceVersions ( versionId ) ON DELETE CASCADE,\n\
        \   FOREIGN KEY ( sourceId ) REFERENCES Pointers ( id ) ON DELETE CASCADE,\n\
        \   FOREIGN KEY ( targetId ) REFERENCES Pointers ( id ) ON DELETE CASCADE,\n\
        \   PRIMARY KEY ( versionId, sourceId )\n\
        \);"
    execute_ conn "\
        \CREATE TABLE IF NOT EXISTS Continuations (\n\
        \   versionId TEXT NOT NULL,\n\
        \   function INTEGER NOT NULL,\n\
        \   next TEXT NOT NULL,\n\
        \   FOREIGN KEY ( versionId ) REFERENCES WorkspaceVersions ( versionId ) ON DELETE CASCADE,\n\
        \   FOREIGN KEY ( function ) REFERENCES Functions ( id ) ON DELETE CASCADE,\n\
        \   PRIMARY KEY ( versionId, function )\n\
        \);"
    execute_ conn "\
        \CREATE TABLE IF NOT EXISTS ContinuationEnvironments (\n\
        \   versionId TEXT NOT NULL,\n\
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
        \   versionId TEXT NOT NULL,\n\
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
        \   t PRIMARY KEY,\n\
        \   processId TEXT NOT NULL,\n\
        \   varEnv TEXT NOT NULL,\n\
        \   funEnv TEXT NOT NULL,\n\
        \   versionId TEXT NOT NULL,\n\
        \   expression TEXT NOT NULL,\n\
        \   continuation TEXT NOT NULL,\n\
        \   FOREIGN KEY ( versionId ) REFERENCES WorkspaceVersions ( versionId ) ON DELETE CASCADE\n\
        \);"
    execute_ conn "\
        \CREATE TABLE IF NOT EXISTS RunQueue (\n\
        \   processId TEXT PRIMARY KEY\n\
        \);"
    execute_ conn "\
        \CREATE TABLE IF NOT EXISTS Sessions (\n\
        \   sessionId TEXT PRIMARY KEY ASC\n\
        \);"
    execute_ conn "\
        \CREATE TABLE IF NOT EXISTS SessionProcesses (\n\
        \   sessionId TEXT NOT NULL,\n\
        \   processId TEXT NOT NULL,\n\
        \   FOREIGN KEY ( sessionId ) REFERENCES Sessions ( sessionId ) ON DELETE CASCADE\n\
        \   PRIMARY KEY ( sessionId ASC, processId ASC )\n\
        \);"
    execute_ conn "\
        \CREATE TABLE IF NOT EXISTS Primitives (\n\
        \   id INTEGER PRIMARY KEY ASC,\n\
        \   pattern TEXT NOT NULL,\n\
        \   body TEXT NOT NULL\n\
        \);"
    execute_ conn "\
        \CREATE VIEW IF NOT EXISTS Latest_WorkspaceVersions AS\n\
        \   SELECT DISTINCT\n\
        \       c.workspaceId,\n\
        \       FIRST_VALUE(c.versionId) OVER w AS versionId,\n\
        \       FIRST_VALUE(c.parentWorkspaceVersionId) OVER w AS parentWorkspaceVersionId,\n\
        \       FIRST_VALUE(c.logicalTime) OVER w AS logicalTime\n\
        \   FROM WorkspaceVersions c\n\
        \   WINDOW w AS (PARTITION BY c.workspaceId ORDER BY c.logicalTime DESC);"
    -- TODO: Can this be done better? Does Sqlite have some equivalent to MSSQL's CROSS APPLY or Postgres' LATERAL?
    execute_ conn "\
        \CREATE VIEW IF NOT EXISTS Prior_Versions AS\n\
        \   WITH RECURSIVE versions(latest, id) AS (\n\
        \       SELECT w.versionId, w.versionId\n\
        \       FROM WorkspaceVersions w\n\
        \       UNION ALL\n\
        \       SELECT v.latest, w.previousVersion\n\
        \       FROM WorkspaceVersions w\n\
        \       INNER JOIN versions v ON w.versionId = v.id\n\
        \       WHERE w.previousVersion IS NOT NULL\n\
        \   )\n\
        \   SELECT latest, id AS versionId FROM versions;"
    -- The Current_* views are aggregating all prior versions of a latest workspace version, e.g. all pointers that were
    -- expanded in any prior version of a latest workspace version.
    execute_ conn "\
        \CREATE VIEW IF NOT EXISTS Current_Links AS\n\
        \   SELECT l.workspaceId, l.versionId, j.sourceId, j.targetId\n\
        \   FROM Latest_WorkspaceVersions l\n\
        \   INNER JOIN Prior_Versions w ON w.latest = l.versionId\n\
        \   INNER JOIN Links j ON j.versionId = w.versionId;"
    execute_ conn "\
        \CREATE VIEW IF NOT EXISTS Current_Messages AS\n\
        \   SELECT m.id, l.workspaceId AS sourceWorkspaceId, l.versionId AS sourceWorkspaceVersionId,\n\
        \                 t.workspaceId AS targetWorkspaceId, m.targetWorkspaceVersionId, m.content\n\
        \   FROM Latest_WorkspaceVersions l\n\
        \   INNER JOIN Prior_Versions w ON w.latest = l.versionId\n\
        \   INNER JOIN Messages m ON m.sourceWorkspaceVersionId = w.versionId\n\
        \   INNER JOIN WorkspaceVersions t ON t.versionId = m.targetWorkspaceVersionId;"
    execute_ conn "\
        \CREATE VIEW IF NOT EXISTS Current_ExpandedPointers AS\n\
        \   SELECT l.workspaceId, l.versionId, p.pointerId\n\
        \   FROM Latest_WorkspaceVersions l\n\
        \   INNER JOIN Prior_Versions w ON w.latest = l.versionId\n\
        \   INNER JOIN ExpandedPointers p ON p.versionId = w.versionId;"
    execute_ conn "\
        \CREATE VIEW IF NOT EXISTS Current_Subquestions AS\n\
        \   SELECT l.workspaceId AS parentWorkspaceId, l.versionId AS parentWorkspaceVersionId, c.workspaceId, c.versionId,\n\
        \          c.logicalTime, cw.questionAsAsked, a.answer\n\
        \   FROM Latest_WorkspaceVersions l\n\
        \   INNER JOIN Prior_Versions w ON w.latest = l.versionId\n\
        \   INNER JOIN Latest_WorkspaceVersions c ON c.parentWorkspaceVersionId = w.versionId\n\
        \   INNER JOIN Workspaces cw ON cw.id = c.workspaceId\n\
        \   LEFT OUTER JOIN Answers a ON a.versionId = c.versionId;"
