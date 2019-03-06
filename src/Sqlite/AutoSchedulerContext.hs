{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module Sqlite.AutoSchedulerContext ( makeSqliteAutoSchedulerContext, makeSqliteAutoSchedulerContext' ) where
import Data.Int ( Int64 ) -- base
import qualified Data.Map as M -- containers
import Database.SQLite.Simple ( Connection, Only(..), NamedParam(..), withTransaction,
                                query, query_, queryNamed, executeMany, executeNamed, execute_, execute, lastInsertRowId ) -- sqlite-simple

import AutoScheduler ( AutoSchedulerContext(..), ProcessId, FunctionId, AddContinuationResult(..),
                       nameToId, idToName, newProcessId, processIdToBuilder, processIdFromText )
import Exp ( Pattern, Exp(..), Exp', EvalState', Name(..), Value, Konts', KontsId', Konts(..),
             parseVarEnv, parseFunEnv,
             varEnvToBuilder, funEnvToBuilder, kont1ToBuilderDB, parseKont1UnsafeDB, expToBuilderDB, expFromDB )
import Message ( PointerRemapping, messageToBuilder, parseMessageUnsafeDB, parsePatternsUnsafe, patternsToBuilder )
import Scheduler ( SchedulerContext(..), SessionId, SyncFunc, sessionIdToBuilder )
import Sqlite.SchedulerContext ( makeSqliteSchedulerContext )
import Util ( toText, Queue, enqueueAsync, parseUnsafe )
import Workspace ( WorkspaceId, workspaceIdFromText, workspaceIdToBuilder )

makeSqliteAutoSchedulerContext :: Connection -> SessionId -> IO (AutoSchedulerContext (Connection, Queue))
makeSqliteAutoSchedulerContext conn sessionId = do
    ctxt <- makeSqliteSchedulerContext conn
    makeSqliteAutoSchedulerContext' ctxt sessionId

makeSqliteAutoSchedulerContext' :: SchedulerContext (Connection, Queue) -> SessionId -> IO (AutoSchedulerContext (Connection, Queue))
makeSqliteAutoSchedulerContext' ctxt sessionId = do
    let (conn, q) = extraContent ctxt
    let sync = doAtomically ctxt

    answerId <- sync $ do
        ss <- queryNamed conn "SELECT f.id \
                              \FROM Functions f \
                              \INNER JOIN Continuations c ON c.function = f.id \
                              \INNER JOIN Trace t ON t.workspaceId = c.workspaceId \
                              \INNER JOIN SessionProcesses s ON s.processId = t.processId \
                              \WHERE s.sessionId = :sessionId AND f.isAnswer = 1 \
                              \LIMIT 1" [":sessionId" := toText (sessionIdToBuilder sessionId)]

        case ss of
            [] -> do
                execute_ conn "INSERT INTO Functions ( isAnswer ) VALUES (1)"
                lastInsertRowId conn
            [Only fId] -> return fId

    return $ AutoSchedulerContext {
                    thisAnswerId = answerId,
                    alternativesFor = alternativesForSqlite sync q conn answerId,
                    allAlternatives = allAlternativesSqlite sync q conn answerId sessionId,
                    addCaseFor = addCaseForSqlite sync q conn answerId,
                    newFunction = newFunctionSqlite sync q conn,
                    linkVars = linkVarsSqlite sync q conn,
                    links = linksSqlite sync q conn,
                    saveContinuation = saveContinuationSqlite sync q conn answerId,
                    loadContinuation = loadContinuationSqlite sync q conn answerId,
                    recordState = recordStateSqlite sync q conn,
                    currentState = currentStateSqlite sync q conn,
                    newProcess = newProcessSqlite sync q conn sessionId,
                    runQueue = runQueueSqlite sync q conn sessionId,
                    terminate = terminateSqlite sync q conn,
                    addContinuationArgument = addContinuationArgumentSqlite sync q conn answerId,
                    continuationArguments = continuationArgumentsSqlite sync q conn answerId,
                    schedulerContext = ctxt
                }

-- NOT CACHEABLE
alternativesForSqlite :: SyncFunc -> Queue -> Connection -> FunctionId -> Name -> IO [([Pattern], Exp')]
alternativesForSqlite sync q conn answerId f = do
    let !fId = nameToId answerId f
    sync $ do
        alts <- query conn "SELECT pattern, body FROM Alternatives WHERE function = ?" (Only fId)
        return $ map (\(ps, e) -> (parsePatternsUnsafe ps, expFromDB e)) alts

-- NOT CACHEABLE
allAlternativesSqlite :: SyncFunc -> Queue -> Connection -> FunctionId -> SessionId -> IO (M.Map Name [([Pattern], Exp')])
allAlternativesSqlite sync q conn answerId sessionId = do
    sync $ do
        alts <- queryNamed conn "SELECT function, pattern, body \
                                \FROM Alternatives \
                                \WHERE function IN (SELECT c.function \
                                \                   FROM Continuations c \
                                \                   INNER JOIN Trace t ON t.workspaceId = c.workspaceId \
                                \                   INNER JOIN SessionProcesses s ON s.processId = t.processId \
                                \                   WHERE s.sessionId = :sessionId) \
                                \ORDER BY rowid ASC" [":sessionId" := toText (sessionIdToBuilder sessionId)]
        return $ M.fromListWith (++) $ map (\(f, ps, e) -> (idToName answerId f, [(parsePatternsUnsafe ps, expFromDB e)])) alts

newFunctionSqlite :: SyncFunc -> Queue -> Connection -> IO Name
newFunctionSqlite sync q conn = do
    sync $ do
        execute_ conn "INSERT INTO Functions DEFAULT VALUES"
        (LOCAL . fromIntegral) <$> lastInsertRowId conn

linkVarsSqlite :: SyncFunc -> Queue -> Connection -> WorkspaceId -> PointerRemapping -> IO ()
linkVarsSqlite sync q conn workspaceId mapping = do
    let !wsIdText = toText (workspaceIdToBuilder workspaceId)
    enqueueAsync q $ do -- TODO: Need 'INSERT OR REPLACE' ?
        executeMany conn "INSERT OR REPLACE INTO Links ( workspaceId, sourceId, targetId ) VALUES (?, ?, ?)" $
            map (\(srcId, tgtId) -> (wsIdText, srcId, tgtId)) (M.toList mapping)

linksSqlite :: SyncFunc -> Queue -> Connection -> WorkspaceId -> IO PointerRemapping
linksSqlite sync q conn workspaceId = do
    let !wsIdText = toText (workspaceIdToBuilder workspaceId)
    sync $ do
        srcTgts <- query conn "SELECT sourceId, targetId FROM Links WHERE workspaceId = ?" (Only wsIdText)
        return $ M.fromList srcTgts

addCaseForSqlite :: SyncFunc -> Queue -> Connection -> FunctionId -> Name -> [Pattern] -> Exp' -> IO ()
addCaseForSqlite sync q conn answerId f patterns e = do
    let !fId = nameToId answerId f
    enqueueAsync q $ do
        -- TODO: XXX This will also need to change to an INSERT OR REPLACE if we just naively have revisits update
        -- the answer and automation. Or maybe we'd need to remove the alternative ahead of time.
        executeNamed conn "INSERT INTO Alternatives ( function, pattern, body ) VALUES (:function, :patterns, :body)" [
                            ":function" := fId,
                            ":patterns" := toText (patternsToBuilder patterns),
                            ":body" := toText (expToBuilderDB e)]

saveContinuationSqlite :: SyncFunc -> Queue -> Connection -> FunctionId -> Konts' -> IO ()
saveContinuationSqlite sync q conn answerId (CallKont funEnv f workspaceId k) = do
    let !fId = nameToId answerId f
    let !wsIdText = toText (workspaceIdToBuilder workspaceId)
    enqueueAsync q $ do
        withTransaction conn $ do
            -- TODO: XXX This will probably need to change to an INSERT OR REPLACE (or maybe an INSERT OR IGNORE) if we just
            -- naively have revisits update the answer and automation.
            executeNamed conn "INSERT OR REPLACE INTO Continuations ( workspaceId, function, next ) VALUES (:workspaceId, :function, :k)" [
                                ":workspaceId" := wsIdText,
                                ":function" := fId,
                                ":k" := toText (kont1ToBuilderDB k)]
            -- TODO: This isn't storing the full funEnv. Can we rebuild a suitable version anyway, by simply taking
            -- all the ContinuationEnvironments associated with this Workspace?
            -- TODO: This also doesn't store any entries with empty VarEnvs. For now, when I consume funEnv, I'll just assume a
            -- lookup that fails means an empty VarEnv.
            executeMany conn "INSERT OR REPLACE INTO ContinuationEnvironments ( workspaceId, function, variable, value ) VALUES (?, ?, ?, ?)"
                (map (\(x, v) -> (wsIdText, fId, x, toText (messageToBuilder v))) -- TODO: Do this outside of the critical section.
                     (M.toList (maybe M.empty id $ M.lookup f funEnv)))

-- CACHEABLE
loadContinuationSqlite :: SyncFunc -> Queue -> Connection -> FunctionId -> KontsId' -> IO Konts'
loadContinuationSqlite sync q conn answerId (workspaceId, f) = do
    let !fId = nameToId answerId f
    let !wsIdText = toText (workspaceIdToBuilder workspaceId)
    sync $ do
        [Only k] <- query conn "SELECT next FROM Continuations WHERE workspaceId = ? AND function = ? LIMIT 1" (wsIdText, fId)
        -- TODO: Here we just get every VarEnv for every function in the Workspace. I don't know if this is right.
        -- It definitely gets more than we need or than was there when the continuation was saved, but that's harmless.
        -- The issue is if we can have branching continuations within a single Workspace. I'm pretty sure the answer
        -- is "no", at least for now.
        vars <- query conn "SELECT function, variable, value FROM ContinuationEnvironments WHERE workspaceId = ?" (Only wsIdText)
        -- TODO: Verify that parseMessageUnsafeDB is the right function to use?
        let !funEnv = M.fromListWith M.union $ map (\(g, x, v) -> (idToName answerId g, M.singleton x (parseMessageUnsafeDB v))) vars
        return (CallKont funEnv f workspaceId (parseKont1UnsafeDB k))

recordStateSqlite :: SyncFunc -> Queue -> Connection -> ProcessId -> EvalState' -> IO ()
recordStateSqlite sync q conn processId (varEnv, funEnv, s, e, k) = do {
    -- To support brute-force revisit, we can add a check here that checks if the state is already in Trace, and if so terminates
    -- processId.
    let { !varEnvText = toText (varEnvToBuilder varEnv);
          !funEnvText = toText (funEnvToBuilder funEnv);
          !expText = toText (expToBuilderDB e);
          !continuationText = toText (kont1ToBuilderDB k);
          !wsIdText = toText (workspaceIdToBuilder s) };

    {--
    seen <- sync $ do {
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
        terminateSqlite sync q conn processId
      else do
    -- -}
        enqueueAsync q $ do
            executeNamed conn "INSERT INTO Trace ( processId, varEnv, funEnv, workspaceId, expression, continuation ) \
                              \VALUES (:processId, :varEnv, :funEnv, :workspaceId, :expression, :continuation)" [
                                ":processId" := toText (processIdToBuilder processId),
                                ":varEnv" := varEnvText, -- TODO: Seems like the varEnv will also be in funEnv
                                ":funEnv" := funEnvText, -- and so doesn't need to be stored separately.
                                ":workspaceId" := wsIdText,
                                ":expression" := expText,
                                ":continuation" := continuationText]
    }

currentStateSqlite :: SyncFunc -> Queue -> Connection -> ProcessId -> IO EvalState'
currentStateSqlite sync q conn pId = do
    sync $ do
        [(varEnv, funEnv, s, e, k)] <- queryNamed conn "SELECT varEnv, funEnv, workspaceId, expression, continuation \
                                                       \FROM Trace \
                                                       \WHERE processId = :processId \
                                                       \ORDER BY t DESC \
                                                       \LIMIT 1" [":processId" := toText (processIdToBuilder pId)]
        return (parseUnsafe parseVarEnv varEnv, parseUnsafe parseFunEnv funEnv, workspaceIdFromText s, expFromDB e, parseKont1UnsafeDB k)

newProcessSqlite :: SyncFunc -> Queue -> Connection -> SessionId -> IO ProcessId
newProcessSqlite sync q conn sessionId = do
    processId <- newProcessId
    enqueueAsync q $ do
        withTransaction conn $ do
            let !pIdText = toText (processIdToBuilder processId)
            execute conn "INSERT INTO RunQueue (processId) VALUES (?)" (Only pIdText)
            executeNamed conn "INSERT INTO SessionProcesses ( sessionId, processId ) VALUES (:sessionId, :processId)" [
                                ":sessionId" := toText (sessionIdToBuilder sessionId),
                                ":processId" := pIdText]
    return processId

runQueueSqlite :: SyncFunc -> Queue -> Connection -> SessionId -> IO [ProcessId]
runQueueSqlite sync q conn sessionId = do
    sync $ do
        map (\(Only pId) -> processIdFromText pId) <$>
            queryNamed conn "SELECT processId \
                            \FROM RunQueue q \
                            \WHERE processId IN (SELECT processId FROM SessionProcesses WHERE sessionId = :sessionId)" [
                                ":sessionId" := toText (sessionIdToBuilder sessionId)]

terminateSqlite :: SyncFunc -> Queue -> Connection -> ProcessId -> IO ()
terminateSqlite sync q conn processId = do
    enqueueAsync q $ do
        executeNamed conn "DELETE FROM RunQueue WHERE processId = :processId" [":processId" := toText (processIdToBuilder processId)]

addContinuationArgumentSqlite :: SyncFunc -> Queue -> Connection -> FunctionId -> KontsId' -> Int -> Value -> IO AddContinuationResult
addContinuationArgumentSqlite sync q conn answerId (workspaceId, f) argNumber v = do
    let !fId = nameToId answerId f
    let !vText = toText (messageToBuilder v)
    let !wsIdText = toText (workspaceIdToBuilder workspaceId)
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
                executeNamed conn "INSERT OR REPLACE INTO ContinuationArguments ( workspaceId, function, argNumber, value ) \
                                  \VALUES (:workspaceId, :function, :argNumber, :value)" [
                                    ":workspaceId" := wsIdText,
                                    ":function" := fId,
                                    ":argNumber" := argNumber,
                                    ":value" := vText]
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
continuationArgumentsSqlite :: SyncFunc -> Queue -> Connection -> FunctionId -> KontsId' -> IO (Konts', [Value])
continuationArgumentsSqlite sync q conn answerId kId@(workspaceId, f) = do
    let !fId = nameToId answerId f
    let !wsIdText = toText (workspaceIdToBuilder workspaceId)
    vs <- sync $ do
        vals <- queryNamed conn "SELECT value \
                                \FROM ContinuationArguments \
                                \WHERE workspaceId = :workspaceId AND function = :function \
                                \ORDER BY argNumber ASC" [":workspaceId" := wsIdText, ":function" := fId]
        return $ map (\(Only v) -> parseMessageUnsafeDB v) vals
    fmap (\k -> (k, vs)) $ loadContinuationSqlite sync q conn answerId kId
