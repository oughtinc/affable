module Caching.AutoSchedulerContext ( makeCachingAutoSchedulerContext ) where
import Control.Concurrent.STM ( TVar, atomically, newTVarIO, readTVar, readTVarIO, writeTVar, modifyTVar' ) -- stm
import Control.Monad ( when ) -- base
import qualified Data.Map as M -- containers

import AutoScheduler ( AutoSchedulerContext(..), ProcessId, FunctionId, AddContinuationResult(..), nameToId, idToName )
import Caching.SchedulerContext ( CacheState(..), priorVersions )
import Exp ( Pattern, Exp(..), Exp', EvalState', Name(..), Value, Konts', KontsId', Konts(..) )
import Message ( PointerRemapping )
import Scheduler ( SchedulerContext(..), SessionId )
import Util ( increment )
import Workspace ( VersionId )

makeCachingAutoSchedulerContext :: CacheState -> AutoSchedulerContext e -> SessionId -> IO (AutoSchedulerContext e)
makeCachingAutoSchedulerContext cache autoCtxt sessionId = do
    let !answerId = thisAnswerId autoCtxt
    atomically $ modifyTVar' (answerFunctionsC cache) (M.insert sessionId answerId)
    altsMap <- readTVarIO (alternativesC cache)
    when (answerId `M.notMember` altsMap) $ do
        alts <- alternativesFor autoCtxt ANSWER
        atomically $ modifyTVar' (alternativesC cache) (M.insert answerId alts)
    return $ AutoSchedulerContext {
                    thisAnswerId = answerId,
                    alternativesFor = alternativesForCaching cache autoCtxt answerId,
                    allAlternatives = allAlternativesCaching cache autoCtxt answerId sessionId,
                    addCaseFor = addCaseForCaching cache autoCtxt answerId,
                    nextFunction = nextFunctionCaching cache autoCtxt,
                    addFunction = addFunctionCaching cache autoCtxt,
                    linkVars = linkVarsCaching cache autoCtxt,
                    links = linksCaching cache autoCtxt,
                    saveContinuation = saveContinuationCaching cache autoCtxt answerId,
                    loadContinuation = loadContinuationCaching cache autoCtxt answerId,
                    recordState = recordStateCaching cache autoCtxt sessionId,
                    currentState = currentStateCaching cache autoCtxt sessionId,
                    newProcess = newProcessCaching cache autoCtxt sessionId,
                    runQueue = runQueueCaching cache autoCtxt sessionId,
                    terminate = terminateCaching cache autoCtxt sessionId,
                    addContinuationArgument = addContinuationArgumentCaching cache autoCtxt answerId,
                    continuationArguments = continuationArgumentsCaching cache autoCtxt answerId,
                    schedulerContext = schedulerContext autoCtxt
                }

-- Backend calls:
--      addFunction - Asynchronous
--      linkVars - Asynchronous
--      addCaseFor - Asynchronous
--      saveContinuation - Asynchronous
--      recordState - Asynchronous
--      newProcess - Asynchronous
--      terminate - Asynchronous
--      addContinuationArgument - Asynchronous

alternativesForCaching :: CacheState -> AutoSchedulerContext e -> FunctionId -> Name -> IO [([Pattern], Exp')]
alternativesForCaching cache autoCtxt answerId f = do
    let !fId = nameToId answerId f
    maybe [] id . M.lookup fId <$> readTVarIO (alternativesC cache)

allAlternativesCaching :: CacheState -> AutoSchedulerContext e -> FunctionId -> SessionId -> IO (M.Map Name [([Pattern], Exp')])
allAlternativesCaching cache autoCtxt answerId sessionId = do -- TODO: Filter to the relevant session.
    M.mapKeys (idToName answerId) <$> readTVarIO (alternativesC cache)

nextFunctionCaching :: CacheState -> AutoSchedulerContext e -> IO Name
nextFunctionCaching cache autoCtxt = do
    name <- LOCAL <$> increment (functionCounter cache)
    addFunction autoCtxt name
    return name

addFunctionCaching :: CacheState -> AutoSchedulerContext e -> Name -> IO ()
addFunctionCaching cache autoCtxt name = return () -- addFunction autoCtxt name

linkVarsCaching :: CacheState -> AutoSchedulerContext e -> VersionId -> PointerRemapping -> IO ()
linkVarsCaching cache autoCtxt versionId mapping = do
    atomically $ modifyTVar' (linksC cache) (M.insertWith M.union versionId mapping)
    linkVars autoCtxt versionId mapping

linksCaching :: CacheState -> AutoSchedulerContext e -> VersionId -> IO PointerRemapping
linksCaching cache autoCtxt versionId = atomically $ do
    workspaces <- readTVar (workspacesC cache)
    let pvs = priorVersions workspaces versionId
    linksMap <- readTVar (linksC cache)
    return $! foldMap (\vId -> maybe M.empty id (M.lookup vId linksMap)) pvs

addCaseForCaching :: CacheState -> AutoSchedulerContext e -> FunctionId -> Name -> [Pattern] -> Exp' -> IO ()
addCaseForCaching cache autoCtxt answerId f patterns e = do
    let !fId = nameToId answerId f
    atomically $ modifyTVar' (alternativesC cache) (M.insertWith (++) fId [(patterns, e)])
    addCaseFor autoCtxt f patterns e

saveContinuationCaching :: CacheState -> AutoSchedulerContext e -> FunctionId -> Konts' -> IO ()
saveContinuationCaching cache autoCtxt answerId k@(CallKont _ f versionId _) = do
    let !fId = nameToId answerId f
    atomically $ modifyTVar' (continuationsC cache) (M.insertWith M.union versionId (M.singleton fId k))
    saveContinuation autoCtxt k

loadContinuationCaching :: CacheState -> AutoSchedulerContext e -> FunctionId -> KontsId' -> IO Konts'
loadContinuationCaching cache autoCtxt answerId (versionId, f) = do
    let !fId = nameToId answerId f
    (\m -> case M.lookup fId m of Just a -> a) . (\m -> case M.lookup versionId m of Just a -> a) <$> readTVarIO (continuationsC cache)

recordStateCaching :: CacheState -> AutoSchedulerContext e -> SessionId -> ProcessId -> EvalState' -> IO ()
recordStateCaching cache autoCtxt sessionId processId state = do
    atomically $ do
        modifyTVar' (traceC cache) ((processId, state):)
        modifyTVar' (runQueueC cache) (M.insertWith M.union sessionId (M.singleton processId state))
    recordState autoCtxt processId state

currentStateCaching :: CacheState -> AutoSchedulerContext e -> SessionId -> ProcessId -> IO EvalState'
currentStateCaching cache autoCtxt sessionId pId
    = (\m -> case M.lookup pId m of Just a -> a) . (\m -> case M.lookup sessionId m of Just a -> a) <$> readTVarIO (runQueueC cache)

newProcessCaching :: CacheState -> AutoSchedulerContext e -> SessionId -> IO ProcessId
newProcessCaching cache autoCtxt sessionId = do
    pId <- newProcess autoCtxt
    atomically $ modifyTVar' (sessionsC cache) (M.insertWith (++) sessionId [pId])
    return pId
    {- TODO: Do something with runQueue.
    enqueueSync q $ do
        withTransaction conn $ do
            [Only processId] <- query_ conn "INSERT INTO RunQueue DEFAULT VALUES RETURNING processId"
            execute conn "INSERT INTO SessionProcesses ( sessionId, processId ) VALUES (?, ?)"
                                (sessionId, processId)
            return processId
    -}

runQueueCaching :: CacheState -> AutoSchedulerContext e -> SessionId -> IO [ProcessId]
runQueueCaching cache autoCtxt sessionId = do
    rq <- readTVarIO (runQueueC cache) -- TODO: Will this miss some processes?
    return $ maybe [] M.keys (M.lookup sessionId rq)

terminateCaching :: CacheState -> AutoSchedulerContext e -> SessionId -> ProcessId -> IO ()
terminateCaching cache autoCtxt sessionId processId = do
    atomically $ modifyTVar' (runQueueC cache) (M.adjust (M.delete processId) sessionId)
    terminate autoCtxt processId

addContinuationArgumentCaching :: CacheState -> AutoSchedulerContext e -> FunctionId -> KontsId' -> Int -> Value -> IO AddContinuationResult
addContinuationArgumentCaching cache autoCtxt answerId kId@(versionId, f) argNumber v = do
    let !fId = nameToId answerId f
    atomically $ modifyTVar' (continuationArgumentsC cache) (M.insertWith M.union (versionId, fId) (M.singleton argNumber v))
    addContinuationArgument autoCtxt kId argNumber v

continuationArgumentsCaching :: CacheState -> AutoSchedulerContext e -> FunctionId -> KontsId' -> IO (Konts', [Value])
continuationArgumentsCaching cache autoCtxt answerId kId@(versionId, f) = do
    let !fId = nameToId answerId f
    vs <- M.elems . (\m -> case M.lookup (versionId, fId) m of Just a -> a) <$> readTVarIO (continuationArgumentsC cache)
    fmap (\k -> (k, vs)) $ loadContinuationCaching cache autoCtxt answerId kId
