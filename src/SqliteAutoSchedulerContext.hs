{-# LANGUAGE OverloadedStrings #-}
module SqliteAutoSchedulerContext ( makeSqliteAutoSchedulerContext, makeSqliteAutoSchedulerContext' ) where
import Data.Int ( Int64 ) -- base
import qualified Data.Map as M -- containers
import Database.SQLite.Simple ( Connection, Only(..), NamedParam(..), withTransaction,
                                query, query_, queryNamed, executeMany, executeNamed, execute_, lastInsertRowId ) -- sqlite-simple

import AutoScheduler ( AutoSchedulerContext(..), ProcessId )
import Exp ( Pattern, Exp(..), Exp', EvalState', Name(..), Value, Konts', KontsId', Konts(..),
             varEnvToBuilder, funEnvToBuilder, kont1ToBuilderDB, parseKont1UnsafeDB, expToBuilderDB, expFromDB )
import Message ( messageToBuilder, parseMessageUnsafeDB, parsePatternsUnsafe, patternsToBuilder )
import Scheduler ( SchedulerContext(..) )
import SqliteSchedulerContext ( makeSqliteSchedulerContext )
import Util ( toText, Lock, withLock )

type FunctionId = Int64

makeSqliteAutoSchedulerContext :: Connection -> IO (AutoSchedulerContext (Connection, Lock))
makeSqliteAutoSchedulerContext conn = do
    ctxt <- makeSqliteSchedulerContext conn
    makeSqliteAutoSchedulerContext' ctxt

makeSqliteAutoSchedulerContext' :: SchedulerContext (Connection, Lock) -> IO (AutoSchedulerContext (Connection, Lock))
makeSqliteAutoSchedulerContext' ctxt = do
    let (conn, lock) = extraContent ctxt

    answerId <- withLock lock $ do
        execute_ conn "INSERT INTO Functions ( isAnswer ) VALUES (1)"
        lastInsertRowId conn

    return $ AutoSchedulerContext {
                    alternativesFor = alternativesForSqlite lock conn answerId,
                    allAlternatives = allAlternativesSqlite lock conn answerId,
                    addCaseFor = addCaseForSqlite lock conn answerId,
                    newFunction = newFunctionSqlite lock conn,
                    saveContinuation = saveContinuationSqlite lock conn answerId,
                    loadContinuation = loadContinuationSqlite lock conn answerId,
                    recordState = recordStateSqlite lock conn,
                    newProcess = newProcessSqlite lock conn,
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
        executeNamed conn "INSERT INTO Alternatives (function, pattern, body) VALUES (:function, :patterns, :body)" [
                            ":function" := fId,
                            ":patterns" := toText (patternsToBuilder patterns),
                            ":body" := toText (expToBuilderDB e)]

saveContinuationSqlite :: Lock -> Connection -> FunctionId -> Konts' -> IO ()
saveContinuationSqlite lock conn answerId (CallKont funEnv f workspaceId k) = do
    let !fId = nameToId answerId f
    withLock lock $ do
        withTransaction conn $ do
            executeNamed conn "INSERT INTO Continuations ( workspaceId, function, next ) VALUES (:workspaceId, :function, :k)" [
                                ":workspaceId" := workspaceId,
                                ":function" := fId,
                                ":k" := toText (kont1ToBuilderDB k)]
            -- TODO: This isn't storing the full funEnv. Can we rebuild a suitable version anyway, by simply taking
            -- all the ContinuationEnvironments associated with this Workspace?
            -- TODO: This also doesn't store any entries with empty VarEnvs. For now, when I consume funEnv, I'll just assume a
            -- lookup that fails means an empty VarEnv.
            executeMany conn "INSERT INTO ContinuationEnvironments ( workspaceId, function, variable, value ) VALUES (?, ?, ?, ?)"
                (map (\(x, v) -> (workspaceId, fId, x, toText (messageToBuilder v)))
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
recordStateSqlite lock conn processId (varEnv, funEnv, s, e, k) = do
    withLock lock $ do
        executeNamed conn "INSERT INTO Trace ( processId, varEnv, funEnv, workspaceId, expression, continuation ) \
                          \VALUES (:processId, :varEnv, :funEnv, :workspaceId, :expression, :continuation)" [
                            ":processId" := processId,
                            ":varEnv" := toText (varEnvToBuilder varEnv), -- TODO: Seems like the varEnv will also be in funEnv
                            ":funEnv" := toText (funEnvToBuilder funEnv), -- and so doesn't need to be stored separately.
                            ":workspaceId" := s,
                            ":expression" := toText (expToBuilderDB e),
                            ":continuation" := toText (kont1ToBuilderDB k)]

newProcessSqlite :: Lock -> Connection -> IO ProcessId
newProcessSqlite lock conn = do
    withLock lock $ do
        execute_ conn "INSERT INTO RunQueue DEFAULT VALUES"
        lastInsertRowId conn

terminateSqlite :: Lock -> Connection -> ProcessId -> IO ()
terminateSqlite lock conn processId = do
    withLock lock $ do
        executeNamed conn "DELETE FROM RunQueue WHERE processId = :processId" [":processId" := processId]

addContinuationArgumentSqlite :: Lock -> Connection -> FunctionId -> KontsId' -> Int -> Value -> IO ()
addContinuationArgumentSqlite lock conn answerId (workspaceId, f) argNumber v = do
    let !fId = nameToId answerId f
    withLock lock $ do
        withTransaction conn $ do
            executeNamed conn "INSERT INTO ContinuationArguments ( workspaceId, function, argNumber, value ) \
                              \VALUES (:workspaceId, :function, :argNumber, :value)" [
                                ":workspaceId" := workspaceId,
                                ":function" := fId,
                                ":argNumber" := argNumber,
                                ":value" := toText (messageToBuilder v)]

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
