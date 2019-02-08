{-# LANGUAGE OverloadedStrings #-}
module SqliteAutoSchedulerContext ( makeSqliteAutoSchedulerContext, makeSqliteAutoSchedulerContext' ) where
import Data.Int ( Int64 ) -- base
import qualified Data.Map as M -- containers
import Database.SQLite.Simple ( Connection, Only(..), NamedParam(..), withTransaction,
                                query, query_, queryNamed, executeMany, executeNamed, execute_, lastInsertRowId ) -- sqlite-simple

import AutoScheduler ( AutoSchedulerContext(..), ProcessId, AddContinuationResult(..) )
import Exp ( Pattern, Exp(..), Exp', EvalState', Name(..), Value, Konts', KontsId', Konts(..),
             parseVarEnv, parseFunEnv,
             varEnvToBuilder, funEnvToBuilder, kont1ToBuilderDB, parseKont1UnsafeDB, expToBuilderDB, expFromDB )
import Message ( messageToBuilder, parseMessageUnsafeDB, parsePatternsUnsafe, patternsToBuilder )
import Scheduler ( SchedulerContext(..) )
import SqliteSchedulerContext ( makeSqliteSchedulerContext )
import Util ( toText, Lock, withLock, parseUnsafe )

type FunctionId = Int64

makeSqliteAutoSchedulerContext :: Connection -> IO (AutoSchedulerContext (Connection, Lock))
makeSqliteAutoSchedulerContext conn = do
    ctxt <- makeSqliteSchedulerContext conn
    makeSqliteAutoSchedulerContext' ctxt

makeSqliteAutoSchedulerContext' :: SchedulerContext (Connection, Lock) -> IO (AutoSchedulerContext (Connection, Lock))
makeSqliteAutoSchedulerContext' ctxt = do
    let (conn, lock) = extraContent ctxt

    q <- runQueueSqlite lock conn -- TODO: This should really be controllable.
    answerId <- withLock lock $ do
        if null q then do
            execute_ conn "INSERT INTO Functions ( isAnswer ) VALUES (1)"
            lastInsertRowId conn
          else do
            [Only fId] <- query_ conn "SELECT id FROM Functions WHERE isAnswer = 1 ORDER BY id DESC LIMIT 1"
            return fId

    return $ AutoSchedulerContext {
                    alternativesFor = alternativesForSqlite lock conn answerId,
                    allAlternatives = allAlternativesSqlite lock conn answerId,
                    addCaseFor = addCaseForSqlite lock conn answerId,
                    newFunction = newFunctionSqlite lock conn,
                    saveContinuation = saveContinuationSqlite lock conn answerId,
                    loadContinuation = loadContinuationSqlite lock conn answerId,
                    recordState = recordStateSqlite lock conn,
                    currentState = currentStateSqlite lock conn,
                    newProcess = newProcessSqlite lock conn,
                    runQueue = runQueueSqlite lock conn,
                    terminate = terminateSqlite lock conn,
                    addContinuationArgument = addContinuationArgumentSqlite lock conn answerId,
                    continuationArguments = continuationArgumentsSqlite lock conn answerId,
                    schedulerContext = ctxt
                }

nameToId :: FunctionId -> Name -> FunctionId
nameToId answerId ANSWER = answerId
nameToId        _ (LOCAL i) = fromIntegral i

idToName :: FunctionId -> FunctionId -> Name
idToName answerId fId | answerId == fId = ANSWER
                      | otherwise = LOCAL (fromIntegral fId)

-- NOT CACHEABLE
alternativesForSqlite :: Lock -> Connection -> FunctionId -> Name -> IO [([Pattern], Exp')]
alternativesForSqlite lock conn answerId f = do
    let !fId = nameToId answerId f
    withLock lock $ do
        alts <- query conn "SELECT pattern, body FROM Alternatives WHERE function = ?" (Only fId)
        return $ map (\(ps, e) -> (parsePatternsUnsafe ps, expFromDB e)) alts

-- NOT CACHEABLE
allAlternativesSqlite :: Lock -> Connection -> FunctionId -> IO (M.Map Name [([Pattern], Exp')])
allAlternativesSqlite lock conn answerId = do
    withLock lock $ do
        alts <- query_ conn "SELECT function, pattern, body FROM Alternatives"
        return $ M.fromListWith (++) $ map (\(f, ps, e) -> (idToName answerId f, [(parsePatternsUnsafe ps, expFromDB e)])) alts

newFunctionSqlite :: Lock -> Connection -> IO Name
newFunctionSqlite lock conn = do
    withLock lock $ do
        execute_ conn "INSERT INTO Functions DEFAULT VALUES"
        (LOCAL . fromIntegral) <$> lastInsertRowId conn

addCaseForSqlite :: Lock -> Connection -> FunctionId -> Name -> [Pattern] -> Exp' -> IO ()
addCaseForSqlite lock conn answerId f patterns e = do
    let !fId = nameToId answerId f
    withLock lock $ do
        -- TODO: XXX This will also need to change to an INSERT OR REPLACE if we just naively have revisits update
        -- the answer and automation. Or maybe we'd need to remove the alternative ahead of time.
        executeNamed conn "INSERT INTO Alternatives ( function, pattern, body ) VALUES (:function, :patterns, :body)" [
                            ":function" := fId,
                            ":patterns" := toText (patternsToBuilder patterns),
                            ":body" := toText (expToBuilderDB e)]

saveContinuationSqlite :: Lock -> Connection -> FunctionId -> Konts' -> IO ()
saveContinuationSqlite lock conn answerId (CallKont funEnv f workspaceId k) = do
    let !fId = nameToId answerId f
    withLock lock $ do
        withTransaction conn $ do
            -- TODO: XXX This will probably need to change to an INSERT OR REPLACE (or maybe an INSERT OR IGNORE) if we just
            -- naively have revisits update the answer and automation.
            executeNamed conn "INSERT OR REPLACE INTO Continuations ( workspaceId, function, next ) VALUES (:workspaceId, :function, :k)" [
                                ":workspaceId" := workspaceId,
                                ":function" := fId,
                                ":k" := toText (kont1ToBuilderDB k)]
            -- TODO: This isn't storing the full funEnv. Can we rebuild a suitable version anyway, by simply taking
            -- all the ContinuationEnvironments associated with this Workspace?
            -- TODO: This also doesn't store any entries with empty VarEnvs. For now, when I consume funEnv, I'll just assume a
            -- lookup that fails means an empty VarEnv.
            executeMany conn "INSERT OR REPLACE INTO ContinuationEnvironments ( workspaceId, function, variable, value ) VALUES (?, ?, ?, ?)"
                (map (\(x, v) -> (workspaceId, fId, x, toText (messageToBuilder v))) -- TODO: Do this outside of the critical section.
                     (M.toList (maybe M.empty id $ M.lookup f funEnv)))

-- CACHEABLE
loadContinuationSqlite :: Lock -> Connection -> FunctionId -> KontsId' -> IO Konts'
loadContinuationSqlite lock conn answerId (workspaceId, f) = do
    let !fId = nameToId answerId f
    withLock lock $ do
        [Only k] <- query conn "SELECT next FROM Continuations WHERE workspaceId = ? AND function = ? LIMIT 1" (workspaceId, fId)
        -- TODO: Here we just get every VarEnv for every function in the Workspace. I don't know if this is right.
        -- It definitely gets more than we need or than was there when the continuation was saved, but that's harmless.
        -- The issue is if we can have branching continuations within a single Workspace. I'm pretty sure the answer
        -- is "no", at least for now.
        vars <- query conn "SELECT function, variable, value FROM ContinuationEnvironments WHERE workspaceId = ?" (Only workspaceId)
        -- TODO: Verify that parseMessageUnsafeDB is the right function to use?
        let !funEnv = M.fromListWith M.union $ map (\(g, x, v) -> (idToName answerId g, M.singleton x (parseMessageUnsafeDB v))) vars
        return (CallKont funEnv f workspaceId (parseKont1UnsafeDB k))

recordStateSqlite :: Lock -> Connection -> ProcessId -> EvalState' -> IO ()
recordStateSqlite lock conn processId (varEnv, funEnv, s, e, k) = do {
    -- To support brute-force revisit, we can add a check here that checks if the state is already in Trace, and if so terminates
    -- processId.
    let { !varEnvText = toText (varEnvToBuilder varEnv);
          !funEnvText = toText (funEnvToBuilder funEnv);
          !expText = toText (expToBuilderDB e);
          !continuationText = toText (kont1ToBuilderDB k) };

    {--
    seen <- withLock lock $ do {
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
        terminateSqlite lock conn processId
      else do
    -- -}
        withLock lock $ do
            executeNamed conn "INSERT INTO Trace ( processId, varEnv, funEnv, workspaceId, expression, continuation ) \
                              \VALUES (:processId, :varEnv, :funEnv, :workspaceId, :expression, :continuation)" [
                                ":processId" := processId,
                                ":varEnv" := varEnvText, -- TODO: Seems like the varEnv will also be in funEnv
                                ":funEnv" := funEnvText, -- and so doesn't need to be stored separately.
                                ":workspaceId" := s,
                                ":expression" := expText,
                                ":continuation" := continuationText]
    }

currentStateSqlite :: Lock -> Connection -> ProcessId -> IO EvalState'
currentStateSqlite lock conn pId = do
    withLock lock $ do
        [(varEnv, funEnv, s, e, k)] <- queryNamed conn "SELECT varEnv, funEnv, workspaceId, expression, continuation \
                                                       \FROM Trace \
                                                       \WHERE processId = :processId \
                                                       \ORDER BY t DESC \
                                                       \LIMIT 1" [":processId" := pId]
        return (parseUnsafe parseVarEnv varEnv, parseUnsafe parseFunEnv funEnv, s, expFromDB e, parseKont1UnsafeDB k)

newProcessSqlite :: Lock -> Connection -> IO ProcessId
newProcessSqlite lock conn = do
    withLock lock $ do
        execute_ conn "INSERT INTO RunQueue DEFAULT VALUES"
        lastInsertRowId conn

runQueueSqlite :: Lock -> Connection -> IO [ProcessId]
runQueueSqlite lock conn = do
    withLock lock $ do
        map (\(Only pId) -> pId) <$> query_ conn "SELECT processId FROM RunQueue"

terminateSqlite :: Lock -> Connection -> ProcessId -> IO ()
terminateSqlite lock conn processId = do
    withLock lock $ do
        executeNamed conn "DELETE FROM RunQueue WHERE processId = :processId" [":processId" := processId]

addContinuationArgumentSqlite :: Lock -> Connection -> FunctionId -> KontsId' -> Int -> Value -> IO AddContinuationResult
addContinuationArgumentSqlite lock conn answerId (workspaceId, f) argNumber v = do
    let !fId = nameToId answerId f
    let !vText = toText (messageToBuilder v)
    withLock lock $ do
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
                                    ":workspaceId" := workspaceId,
                                    ":function" := fId,
                                    ":argNumber" := argNumber,
                                    ":value" := vText]
                return NEW
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

-- NOT CACHEABLE
continuationArgumentsSqlite :: Lock -> Connection -> FunctionId -> KontsId' -> IO (Konts', [Value])
continuationArgumentsSqlite lock conn answerId kId@(workspaceId, f) = do
    let !fId = nameToId answerId f
    vs <- withLock lock $ do
        vals <- queryNamed conn "SELECT value \
                                \FROM ContinuationArguments \
                                \WHERE workspaceId = :workspaceId AND function = :function \
                                \ORDER BY argNumber ASC" [":workspaceId" := workspaceId, ":function" := fId]
        return $ map (\(Only v) -> parseMessageUnsafeDB v) vals
    fmap (\k -> (k, vs)) $ loadContinuationSqlite lock conn answerId kId
