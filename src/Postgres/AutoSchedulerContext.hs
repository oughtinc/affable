{-# LANGUAGE OverloadedStrings #-}
module Postgres.AutoSchedulerContext ( makePostgresAutoSchedulerContext, makePostgresAutoSchedulerContext' ) where
import Data.Int ( Int64 ) -- base
import qualified Data.Map as M -- containers
import Database.PostgreSQL.Simple ( Connection, Only(..), withTransaction, query, query_, executeMany, execute_, execute ) -- postgresql-simple

import AutoScheduler ( AutoSchedulerContext(..), ProcessId, FunctionId, AddContinuationResult(..), nameToId, idToName, newProcessId )
import Exp ( Pattern, Exp(..), Exp', EvalState', Name(..), Value, Konts', KontsId', Konts(..),
             parseVarEnv, parseFunEnv,
             varEnvToBuilder, funEnvToBuilder, kont1ToBuilderDB, parseKont1UnsafeDB, expToBuilderDB, expFromDB )
import Message ( PointerRemapping, messageToBuilder, parseMessageUnsafeDB, parsePatternsUnsafe, patternsToBuilder )
import Scheduler ( SchedulerContext(..), SessionId )
import Postgres.SchedulerContext ( makePostgresSchedulerContext )
import Util ( toText, Queue, enqueueAsync, enqueueSync, parseUnsafe )
import Workspace ( WorkspaceId )

makePostgresAutoSchedulerContext :: Connection -> SessionId -> IO (AutoSchedulerContext (Connection, Queue))
makePostgresAutoSchedulerContext conn sessionId = do
    ctxt <- makePostgresSchedulerContext conn
    makePostgresAutoSchedulerContext' ctxt sessionId

makePostgresAutoSchedulerContext' :: SchedulerContext (Connection, Queue) -> SessionId -> IO (AutoSchedulerContext (Connection, Queue))
makePostgresAutoSchedulerContext' ctxt sessionId = do
    let (conn, q) = extraContent ctxt

    answerId <- enqueueSync q $ do
        ss <- query conn "SELECT f.id \
                         \FROM Functions f \
                         \INNER JOIN Continuations c ON c.function = f.id \
                         \INNER JOIN Trace t ON t.workspaceId = c.workspaceId \
                         \INNER JOIN SessionProcesses s ON s.processId = t.processId \
                         \WHERE s.sessionId = ? AND f.isAnswer = 1 \
                         \LIMIT 1" (Only sessionId)

        case ss of
            [] -> do
                [Only fId] <- query_ conn "INSERT INTO Functions ( isAnswer ) VALUES (1) RETURNING id"
                return fId
            [Only fId] -> return fId

    return $ AutoSchedulerContext {
                    thisAnswerId = answerId,
                    alternativesFor = alternativesForPostgres q conn answerId,
                    allAlternatives = allAlternativesPostgres q conn answerId sessionId,
                    addCaseFor = addCaseForPostgres q conn answerId,
                    newFunction = newFunctionPostgres q conn,
                    linkVars = linkVarsPostgres q conn,
                    links = linksPostgres q conn,
                    saveContinuation = saveContinuationPostgres q conn answerId,
                    loadContinuation = loadContinuationPostgres q conn answerId,
                    recordState = recordStatePostgres q conn,
                    currentState = currentStatePostgres q conn,
                    newProcess = newProcessPostgres q conn sessionId,
                    runQueue = runQueuePostgres q conn sessionId,
                    terminate = terminatePostgres q conn,
                    addContinuationArgument = addContinuationArgumentPostgres q conn answerId,
                    continuationArguments = continuationArgumentsPostgres q conn answerId,
                    schedulerContext = ctxt
                }

-- NOT CACHEABLE
alternativesForPostgres :: Queue -> Connection -> FunctionId -> Name -> IO [([Pattern], Exp')]
alternativesForPostgres q conn answerId f = do
    let !fId = nameToId answerId f
    enqueueSync q $ do
        alts <- query conn "SELECT pattern, body FROM Alternatives WHERE function = ?" (Only fId)
        return $ map (\(ps, e) -> (parsePatternsUnsafe ps, expFromDB e)) alts

-- NOT CACHEABLE
allAlternativesPostgres :: Queue -> Connection -> FunctionId -> SessionId -> IO (M.Map Name [([Pattern], Exp')])
allAlternativesPostgres q conn answerId sessionId = do
    enqueueSync q $ do
        alts <- query conn "SELECT function, pattern, body \
                           \FROM Alternatives \
                           \WHERE function IN (SELECT c.function \
                           \                   FROM Continuations c \
                           \                   INNER JOIN Trace t ON t.workspaceId = c.workspaceId \
                           \                   INNER JOIN SessionProcesses s ON s.processId = t.processId \
                           \                   WHERE s.sessionId = ?)"
                           (Only sessionId) -- TODO: ORDER BY
        return $ M.fromListWith (++) $ map (\(f, ps, e) -> (idToName answerId f, [(parsePatternsUnsafe ps, expFromDB e)])) alts

newFunctionPostgres :: Queue -> Connection -> IO Name
newFunctionPostgres q conn = do
    enqueueSync q $ do
        [Only fId] <- query_ conn "INSERT INTO Functions DEFAULT VALUES RETURNING id"
        return (LOCAL fId)

linkVarsPostgres :: Queue -> Connection -> WorkspaceId -> PointerRemapping -> IO ()
linkVarsPostgres q conn workspaceId mapping = do
    enqueueAsync q $ do
        () <$ executeMany conn "INSERT INTO Links ( workspaceId, sourceId, targetId ) VALUES (?, ?, ?) \
                               \ON CONFLICT (workspaceId, sourceId) DO UPDATE SET targetId = excluded.targetId"
                (map (\(srcId, tgtId) -> (workspaceId, srcId, tgtId)) (M.toList mapping))

linksPostgres :: Queue -> Connection -> WorkspaceId -> IO PointerRemapping
linksPostgres q conn workspaceId = do
    enqueueSync q $ do
        srcTgts <- query conn "SELECT sourceId, targetId FROM Links WHERE workspaceId = ?" (Only workspaceId)
        return $ M.fromList srcTgts

addCaseForPostgres :: Queue -> Connection -> FunctionId -> Name -> [Pattern] -> Exp' -> IO ()
addCaseForPostgres q conn answerId f patterns e = do
    let !fId = nameToId answerId f
    enqueueAsync q $ do
        -- TODO: XXX This will also need to change to an INSERT OR REPLACE if we just naively have revisits update
        -- the answer and automation. Or maybe we'd need to remove the alternative ahead of time.
        () <$ execute conn "INSERT INTO Alternatives ( function, pattern, body ) VALUES (?, ?, ?)"
                            (fId, toText (patternsToBuilder patterns), toText (expToBuilderDB e))

saveContinuationPostgres :: Queue -> Connection -> FunctionId -> Konts' -> IO ()
saveContinuationPostgres q conn answerId (CallKont funEnv f workspaceId k) = do
    let !fId = nameToId answerId f
    enqueueAsync q $ do
        withTransaction conn $ do
            -- TODO: XXX This will probably need to change to an INSERT OR REPLACE (or maybe an INSERT OR IGNORE) if we just
            -- naively have revisits update the answer and automation.
            execute conn "INSERT INTO Continuations ( workspaceId, function, next ) VALUES (?, ?, ?) \
                         \ON CONFLICT (workspaceId, function) DO UPDATE SET next = excluded.next"
                                (workspaceId, fId, toText (kont1ToBuilderDB k))
            -- TODO: This isn't storing the full funEnv. Can we rebuild a suitable version anyway, by simply taking
            -- all the ContinuationEnvironments associated with this Workspace?
            -- TODO: This also doesn't store any entries with empty VarEnvs. For now, when I consume funEnv, I'll just assume a
            -- lookup that fails means an empty VarEnv.
            () <$ executeMany conn "INSERT INTO ContinuationEnvironments ( workspaceId, function, variable, value ) VALUES (?, ?, ?, ?) \
                                   \ON CONFLICT(workspaceId, function, variable) DO UPDATE SET value = excluded.value"
                    (map (\(x, v) -> (workspaceId, fId, x, toText (messageToBuilder v))) -- TODO: Do this outside of the critical section.
                         (M.toList (maybe M.empty id $ M.lookup f funEnv)))

-- CACHEABLE
loadContinuationPostgres :: Queue -> Connection -> FunctionId -> KontsId' -> IO Konts'
loadContinuationPostgres q conn answerId (workspaceId, f) = do
    let !fId = nameToId answerId f
    enqueueSync q $ do
        [Only k] <- query conn "SELECT next FROM Continuations WHERE workspaceId = ? AND function = ? LIMIT 1" (workspaceId, fId)
        -- TODO: Here we just get every VarEnv for every function in the Workspace. I don't know if this is right.
        -- It definitely gets more than we need or than was there when the continuation was saved, but that's harmless.
        -- The issue is if we can have branching continuations within a single Workspace. I'm pretty sure the answer
        -- is "no", at least for now.
        vars <- query conn "SELECT function, variable, value FROM ContinuationEnvironments WHERE workspaceId = ?" (Only workspaceId)
        -- TODO: Verify that parseMessageUnsafeDB is the right function to use?
        let !funEnv = M.fromListWith M.union $ map (\(g, x, v) -> (idToName answerId g, M.singleton x (parseMessageUnsafeDB v))) vars
        return (CallKont funEnv f workspaceId (parseKont1UnsafeDB k))

recordStatePostgres :: Queue -> Connection -> ProcessId -> EvalState' -> IO ()
recordStatePostgres q conn processId (varEnv, funEnv, s, e, k) = do {
    -- To support brute-force revisit, we can add a check here that checks if the state is already in Trace, and if so terminates
    -- processId.
    let { !varEnvText = toText (varEnvToBuilder varEnv);
          !funEnvText = toText (funEnvToBuilder funEnv);
          !expText = toText (expToBuilderDB e);
          !continuationText = toText (kont1ToBuilderDB k) };

    {--
    seen <- enqueueSync q $ do {
        rs <- queryNamed conn "SELECT 1 \
                              \FROM Trace \
                              \WHERE varEnv = :varEnv AND funEnv = :funEnv AND workspaceId = :workspaceId \
                              \  AND expression = :expression AND continuation = :continuation \
                              \LIMIT 1" [
                                ":varEnv" := varEnvText,
                                ":funEnv" := funEnvText,
                                ":workspaceId" := s,
                                ":expression" := expText,
                                ":continuation" := continuationText];
        return (not (null (rs :: [Only Int64]))) };

    if seen then do
        terminatePostgres q conn processId
      else do
    -- -}
        enqueueAsync q $ do
            () <$ execute conn "INSERT INTO Trace ( processId, varEnv, funEnv, workspaceId, expression, continuation ) VALUES (?, ?, ?, ?, ?, ?)"
                                (processId,
                                 varEnvText, -- TODO: Seems like the varEnv will also be in funEnv
                                 funEnvText, -- and so doesn't need to be stored separately.
                                 s,
                                 expText,
                                 continuationText)
    }

currentStatePostgres :: Queue -> Connection -> ProcessId -> IO EvalState'
currentStatePostgres q conn pId = do
    enqueueSync q $ do
        [(varEnv, funEnv, s, e, k)] <- query conn "SELECT varEnv, funEnv, workspaceId, expression, continuation \
                                                  \FROM Trace \
                                                  \WHERE processId = ? \
                                                  \ORDER BY t DESC \
                                                  \LIMIT 1" (Only pId)
        return (parseUnsafe parseVarEnv varEnv, parseUnsafe parseFunEnv funEnv, s, expFromDB e, parseKont1UnsafeDB k)

newProcessPostgres :: Queue -> Connection -> SessionId -> IO ProcessId
newProcessPostgres q conn sessionId = do
    processId <- newProcessId
    enqueueAsync q $ do
        withTransaction conn $ do
            execute conn "INSERT INTO RunQueue (processId) VALUES (?)" (Only processId)
            () <$ execute conn "INSERT INTO SessionProcesses ( sessionId, processId ) VALUES (?, ?)" (sessionId, processId)
    return processId

runQueuePostgres :: Queue -> Connection -> SessionId -> IO [ProcessId]
runQueuePostgres q conn sessionId = do
    enqueueSync q $ do
        map (\(Only pId) -> pId) <$> query conn "SELECT processId \
                                                \FROM RunQueue q \
                                                \WHERE processId IN (SELECT processId FROM SessionProcesses WHERE sessionId = ?)"
                                                        (Only sessionId)

terminatePostgres :: Queue -> Connection -> ProcessId -> IO ()
terminatePostgres q conn processId = do
    enqueueAsync q $ do
        () <$ execute conn "DELETE FROM RunQueue WHERE processId = ?" (Only processId)

addContinuationArgumentPostgres :: Queue -> Connection -> FunctionId -> KontsId' -> Int -> Value -> IO AddContinuationResult
addContinuationArgumentPostgres q conn answerId (workspaceId, f) argNumber v = do
    let !fId = nameToId answerId f
    let !vText = toText (messageToBuilder v)
    enqueueAsync q $ do
        {-
        vs <- queryNamed conn "SELECT value \
                              \FROM ContinuationArguments \
                              \WHERE workspaceId = :workspaceId AND function = :function AND argNumber = :argNumber \
                              \LIMIT 1" [
                            ":workspaceId" := workspaceId,
                            ":function" := fId,
                            ":argNumber" := argNumber]
        case vs of
            [] -> do
        -}
                -- TODO: Can remove the 'OR REPLACE' if the other code is uncommented.
                () <$ execute conn "INSERT INTO ContinuationArguments ( workspaceId, function, argNumber, value ) VALUES (?, ?, ?, ?) \
                                   \ON CONFLICT (workspaceId, function, argNumber) DO UPDATE SET value = excluded.value"
                                    (workspaceId, fId, argNumber, vText)
                -- return NEW
        {-
            [Only v'] | vText == v' -> return SAME
                      | otherwise -> do -- TODO: Formulate an approach that doesn't involve in-place updates.
                            executeNamed conn "UPDATE ContinuationArguments SET value = :value \
                                              \WHERE workspaceId =  :workspaceId AND function = :function AND argNumber = :argNumber" [
                                                ":workspaceId" := workspaceId,
                                                ":function" := fId,
                                                ":argNumber" := argNumber,
                                                ":value" := vText]
                            return REPLACED
        -}
    return NEW

-- NOT CACHEABLE
continuationArgumentsPostgres :: Queue -> Connection -> FunctionId -> KontsId' -> IO (Konts', [Value])
continuationArgumentsPostgres q conn answerId kId@(workspaceId, f) = do
    let !fId = nameToId answerId f
    vs <- enqueueSync q $ do
        vals <- query conn "SELECT value \
                           \FROM ContinuationArguments \
                           \WHERE workspaceId = ? AND function = ? \
                           \ORDER BY argNumber ASC" (workspaceId, fId)
        return $ map (\(Only v) -> parseMessageUnsafeDB v) vals
    fmap (\k -> (k, vs)) $ loadContinuationPostgres q conn answerId kId
