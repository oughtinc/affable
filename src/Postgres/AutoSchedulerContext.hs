{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module Postgres.AutoSchedulerContext ( makePostgresAutoSchedulerContext ) where
import Control.Concurrent ( myThreadId ) -- base
import Data.Int ( Int64 ) -- base
import qualified Data.Map as M -- containers
import Database.PostgreSQL.Simple ( Connection, Only(..), withTransaction, query, query_, executeMany, execute_, execute ) -- postgresql-simple

import AutoScheduler ( AutoSchedulerContext(..), ProcessId, FunctionId, AddContinuationResult(..), nameToId, idToName, newProcessId )
import Exp ( Pattern, Exp(..), Exp', EvalState', Name(..), Value, Konts', KontsId', Konts(..),
             parseVarEnv, parseFunEnv,
             varEnvToBuilder, funEnvToBuilder, kont1ToBuilderDB, parseKont1UnsafeDB, expToBuilderDB, expFromDB )
import Message ( PointerRemapping, messageToBuilder, parseMessageUnsafeDB, parsePatternsUnsafe, patternsToBuilder )
import Scheduler ( SchedulerContext(..), SessionId, SyncFunc, AsyncFunc )
import Postgres.SchedulerContext ( makePostgresSchedulerContext )
import Util ( toText, Queue, enqueueSync, enqueueAsync, parseUnsafe )
import Workspace ( VersionId )

makePostgresAutoSchedulerContext :: SchedulerContext (Connection, Queue) -> SessionId -> IO (AutoSchedulerContext (Connection, Queue))
makePostgresAutoSchedulerContext ctxt sessionId = do
    let (conn, q) = extraContent ctxt
    let sync = doAtomically ctxt
    qThreadId <- enqueueSync q myThreadId
    let async action = do
            tId <- myThreadId
            if qThreadId == tId then action -- If we're already on the Queue's thread, no need to enqueue.
                                else enqueueAsync q action

    answerId <- sync $ do
        ss <- query conn "SELECT f.id \
                         \FROM Functions f \
                         \INNER JOIN Continuations c ON c.function = f.id \
                         \INNER JOIN Trace t ON t.versionId = c.versionId \
                         \INNER JOIN SessionProcesses s ON s.processId = t.processId \
                         \WHERE s.sessionId = ? AND f.isAnswer = 1 \
                         \LIMIT 1" (Only sessionId)

        case ss of
            [] -> do
                [Only fId] <- query_ conn "INSERT INTO Functions ( isAnswer ) VALUES (1) ON CONFLICT (id) DO UPDATE SET isAnswer = 1 RETURNING id"
                return fId
            [Only fId] -> return fId

    return $ AutoSchedulerContext {
                    thisAnswerId = answerId,
                    alternativesFor = alternativesForPostgres sync async conn answerId,
                    allAlternatives = allAlternativesPostgres sync async conn answerId sessionId,
                    addCaseFor = addCaseForPostgres sync async conn answerId,
                    nextFunction = nextFunctionPostgres sync async conn,
                    addFunction = addFunctionPostgres sync async conn answerId,
                    linkVars = linkVarsPostgres sync async conn,
                    links = linksPostgres sync async conn,
                    saveContinuation = saveContinuationPostgres sync async conn answerId,
                    loadContinuation = loadContinuationPostgres sync async conn answerId,
                    recordState = recordStatePostgres sync async conn,
                    currentState = currentStatePostgres sync async conn,
                    newProcess = newProcessPostgres sync async conn sessionId,
                    runQueue = runQueuePostgres sync async conn sessionId,
                    terminate = terminatePostgres sync async conn,
                    addContinuationArgument = addContinuationArgumentPostgres sync async conn answerId,
                    continuationArguments = continuationArgumentsPostgres sync async conn answerId,
                    schedulerContext = ctxt
                }

-- NOT CACHEABLE
alternativesForPostgres :: SyncFunc -> AsyncFunc -> Connection -> FunctionId -> Name -> IO [([Pattern], Exp')]
alternativesForPostgres sync async conn answerId f = do
    let !fId = nameToId answerId f
    sync $ do
        alts <- query conn "SELECT pattern, body FROM Alternatives WHERE function = ?" (Only fId)
        return $ map (\(ps, e) -> (parsePatternsUnsafe ps, expFromDB e)) alts

-- NOT CACHEABLE
allAlternativesPostgres :: SyncFunc -> AsyncFunc -> Connection -> FunctionId -> SessionId -> IO (M.Map Name [([Pattern], Exp')])
allAlternativesPostgres sync async conn answerId sessionId = do
    sync $ do
        alts <- query conn "SELECT function, pattern, body \
                           \FROM Alternatives \
                           \WHERE function IN (SELECT c.function \
                           \                   FROM Continuations c \
                           \                   INNER JOIN Trace t ON t.versionId = c.versionId \
                           \                   INNER JOIN SessionProcesses s ON s.processId = t.processId \
                           \                   WHERE s.sessionId = ?)"
                           (Only sessionId) -- TODO: ORDER BY
        return $ M.fromListWith (++) $ map (\(f, ps, e) -> (idToName answerId f, [(parsePatternsUnsafe ps, expFromDB e)])) alts

nextFunctionPostgres :: SyncFunc -> AsyncFunc -> Connection -> IO Name
nextFunctionPostgres sync async conn = do
    sync $ do
        [Only fId] <- query_ conn "INSERT INTO Functions DEFAULT VALUES RETURNING id"
        return (LOCAL fId)

addFunctionPostgres :: SyncFunc -> AsyncFunc -> Connection -> FunctionId -> Name -> IO ()
addFunctionPostgres sync async conn answerId name = do
    async $ do
        () <$ execute conn "INSERT INTO Functions ( id ) VALUES (?) ON CONFLICT (id) DO NOTHING" (Only (nameToId answerId name))

linkVarsPostgres :: SyncFunc -> AsyncFunc -> Connection -> VersionId -> PointerRemapping -> IO ()
linkVarsPostgres sync async conn versionId mapping = do
    -- TODO: Need to handle versions here? Probably.
    async $ do
        () <$ executeMany conn "INSERT INTO Links ( versionId, sourceId, targetId ) VALUES (?, ?, ?) \
                               \ON CONFLICT (versionId, sourceId) DO UPDATE SET targetId = excluded.targetId"
                (map (\(srcId, tgtId) -> (versionId, srcId, tgtId)) (M.toList mapping))

linksPostgres :: SyncFunc -> AsyncFunc -> Connection -> VersionId -> IO PointerRemapping
linksPostgres sync async conn versionId = do
    sync $ do
        srcTgts <- query conn "SELECT sourceId, targetId FROM Links WHERE versionId = ?" (Only versionId)
        return $ M.fromList srcTgts

addCaseForPostgres :: SyncFunc -> AsyncFunc -> Connection -> FunctionId -> Name -> [Pattern] -> Exp' -> IO ()
addCaseForPostgres sync async conn answerId f patterns e = do
    let !fId = nameToId answerId f
    async $ do
        -- TODO: XXX This will also need to change to an INSERT OR REPLACE if we just naively have revisits update
        -- the answer and automation. Or maybe we'd need to remove the alternative ahead of time.
        () <$ execute conn "INSERT INTO Alternatives ( function, pattern, body ) VALUES (?, ?, ?)"
                            (fId, toText (patternsToBuilder patterns), toText (expToBuilderDB e))

saveContinuationPostgres :: SyncFunc -> AsyncFunc -> Connection -> FunctionId -> Konts' -> IO ()
saveContinuationPostgres sync async conn answerId (CallKont funEnv f versionId k) = do
    let !fId = nameToId answerId f
    async $ do
        withTransaction conn $ do
            -- TODO: XXX This will probably need to change to an INSERT OR REPLACE (or maybe an INSERT OR IGNORE) if we just
            -- naively have revisits update the answer and automation.
            execute conn "INSERT INTO Continuations ( versionId, function, next ) VALUES (?, ?, ?) \
                         \ON CONFLICT (versionId, function) DO UPDATE SET next = excluded.next"
                                (versionId, fId, toText (kont1ToBuilderDB k))
            -- TODO: This isn't storing the full funEnv. Can we rebuild a suitable version anyway, by simply taking
            -- all the ContinuationEnvironments associated with this Workspace?
            -- TODO: This also doesn't store any entries with empty VarEnvs. For now, when I consume funEnv, I'll just assume a
            -- lookup that fails means an empty VarEnv.
            () <$ executeMany conn "INSERT INTO ContinuationEnvironments ( versionId, function, variable, value ) VALUES (?, ?, ?, ?) \
                                   \ON CONFLICT(versionId, function, variable) DO UPDATE SET value = excluded.value"
                    (map (\(x, v) -> (versionId, fId, x, toText (messageToBuilder v))) -- TODO: Do this outside of the critical section.
                         (M.toList (maybe M.empty id $ M.lookup f funEnv)))

-- CACHEABLE
loadContinuationPostgres :: SyncFunc -> AsyncFunc -> Connection -> FunctionId -> KontsId' -> IO Konts'
loadContinuationPostgres sync async conn answerId (versionId, f) = do
    let !fId = nameToId answerId f
    sync $ do
        [Only k] <- query conn "SELECT next FROM Continuations WHERE versionId = ? AND function = ? LIMIT 1" (versionId, fId)
        -- TODO: Here we just get every VarEnv for every function in the Workspace. I don't know if this is right.
        -- It definitely gets more than we need or than was there when the continuation was saved, but that's harmless.
        -- The issue is if we can have branching continuations within a single Workspace. I'm pretty sure the answer
        -- is "no", at least for now.
        vars <- query conn "SELECT function, variable, value FROM ContinuationEnvironments WHERE versionId = ?" (Only versionId)
        -- TODO: Verify that parseMessageUnsafeDB is the right function to use?
        let !funEnv = M.fromListWith M.union $ map (\(g, x, v) -> (idToName answerId g, M.singleton x (parseMessageUnsafeDB v))) vars
        return (CallKont funEnv f versionId (parseKont1UnsafeDB k))

recordStatePostgres :: SyncFunc -> AsyncFunc -> Connection -> ProcessId -> EvalState' -> IO ()
recordStatePostgres sync async conn processId (varEnv, funEnv, s, e, k) = do {
    -- To support brute-force revisit, we can add a check here that checks if the state is already in Trace, and if so terminates
    -- processId.
    let { !varEnvText = toText (varEnvToBuilder varEnv);
          !funEnvText = toText (funEnvToBuilder funEnv);
          !expText = toText (expToBuilderDB e);
          !continuationText = toText (kont1ToBuilderDB k) };

    {--
    seen <- sync $ do {
        rs <- queryNamed conn "SELECT 1 \
                              \FROM Trace \
                              \WHERE varEnv = :varEnv AND funEnv = :funEnv AND versionId = :versionId \
                              \  AND expression = :expression AND continuation = :continuation \
                              \LIMIT 1" [
                                ":varEnv" := varEnvText,
                                ":funEnv" := funEnvText,
                                ":versionId" := s,
                                ":expression" := expText,
                                ":continuation" := continuationText];
        return (not (null (rs :: [Only Int64]))) };

    if seen then do
        terminatePostgres sync async conn processId
      else do
    -- -}
        async $ do
            () <$ execute conn "INSERT INTO Trace ( processId, varEnv, funEnv, versionId, expression, continuation ) VALUES (?, ?, ?, ?, ?, ?)"
                                (processId,
                                 varEnvText, -- TODO: Seems like the varEnv will also be in funEnv
                                 funEnvText, -- and so doesn't need to be stored separately.
                                 s,
                                 expText,
                                 continuationText)
    }

currentStatePostgres :: SyncFunc -> AsyncFunc -> Connection -> ProcessId -> IO EvalState'
currentStatePostgres sync async conn pId = do
    sync $ do
        [(varEnv, funEnv, s, e, k)] <- query conn "SELECT varEnv, funEnv, versionId, expression, continuation \
                                                  \FROM Trace \
                                                  \WHERE processId = ? \
                                                  \ORDER BY t DESC \
                                                  \LIMIT 1" (Only pId)
        return (parseUnsafe parseVarEnv varEnv, parseUnsafe parseFunEnv funEnv, s, expFromDB e, parseKont1UnsafeDB k)

newProcessPostgres :: SyncFunc -> AsyncFunc -> Connection -> SessionId -> IO ProcessId
newProcessPostgres sync async conn sessionId = do
    processId <- newProcessId
    async $ do
        withTransaction conn $ do
            execute conn "INSERT INTO RunQueue ( processId ) VALUES (?)" (Only processId)
            () <$ execute conn "INSERT INTO SessionProcesses ( sessionId, processId ) VALUES (?, ?)" (sessionId, processId)
    return processId

runQueuePostgres :: SyncFunc -> AsyncFunc -> Connection -> SessionId -> IO [ProcessId]
runQueuePostgres sync async conn sessionId = do
    sync $ do
        map (\(Only pId) -> pId) <$> query conn "SELECT processId \
                                                \FROM RunQueue q \
                                                \WHERE processId IN (SELECT processId FROM SessionProcesses WHERE sessionId = ?)"
                                                        (Only sessionId)

terminatePostgres :: SyncFunc -> AsyncFunc -> Connection -> ProcessId -> IO ()
terminatePostgres sync async conn processId = do
    async $ do
        () <$ execute conn "DELETE FROM RunQueue WHERE processId = ?" (Only processId)

addContinuationArgumentPostgres :: SyncFunc -> AsyncFunc -> Connection -> FunctionId -> KontsId' -> Int -> Value -> IO AddContinuationResult
addContinuationArgumentPostgres sync async conn answerId (versionId, f) argNumber v = do
    let !fId = nameToId answerId f
    let !vText = toText (messageToBuilder v)
    async $ do
        {-
        vs <- queryNamed conn "SELECT value \
                              \FROM ContinuationArguments \
                              \WHERE versionId = :versionId AND function = :function AND argNumber = :argNumber \
                              \LIMIT 1" [
                            ":versionId" := versionId,
                            ":function" := fId,
                            ":argNumber" := argNumber]
        case vs of
            [] -> do
        -}
                -- TODO: Can remove the 'OR REPLACE' if the other code is uncommented.
                () <$ execute conn "INSERT INTO ContinuationArguments ( versionId, function, argNumber, value ) VALUES (?, ?, ?, ?) \
                                   \ON CONFLICT (versionId, function, argNumber) DO UPDATE SET value = excluded.value"
                                    (versionId, fId, argNumber, vText)
                -- return NEW
        {-
            [Only v'] | vText == v' -> return SAME
                      | otherwise -> do -- TODO: Formulate an approach that doesn't involve in-place updates.
                            executeNamed conn "UPDATE ContinuationArguments SET value = :value \
                                              \WHERE versionId =  :versionId AND function = :function AND argNumber = :argNumber" [
                                                ":versionId" := versionId,
                                                ":function" := fId,
                                                ":argNumber" := argNumber,
                                                ":value" := vText]
                            return REPLACED
        -}
    return NEW

-- NOT CACHEABLE
continuationArgumentsPostgres :: SyncFunc -> AsyncFunc -> Connection -> FunctionId -> KontsId' -> IO (Konts', [Value])
continuationArgumentsPostgres sync async conn answerId kId@(versionId, f) = do
    let !fId = nameToId answerId f
    vs <- sync $ do
        vals <- query conn "SELECT value \
                           \FROM ContinuationArguments \
                           \WHERE versionId = ? AND function = ? \
                           \ORDER BY argNumber ASC" (versionId, fId)
        return $ map (\(Only v) -> parseMessageUnsafeDB v) vals
    fmap (\k -> (k, vs)) $ loadContinuationPostgres sync async conn answerId kId
