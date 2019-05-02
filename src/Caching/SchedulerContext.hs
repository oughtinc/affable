{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module Caching.SchedulerContext ( CacheState(..), createCache, makeCachingSchedulerContext ) where
import Control.Concurrent.STM ( TVar, atomically, newTVarIO, readTVar, readTVarIO, writeTVar, modifyTVar' ) -- stm
import qualified Data.Map as M -- containers
import Data.Maybe ( mapMaybe ) -- base

import AutoScheduler ( FunctionId, ProcessId )
import DatabaseContext ( Snapshot(..) )
import Exp ( Pattern, Exp', Konts', EvalState' )
import Message ( Message(..), Pointer, PointerEnvironment, PointerRemapping, applyLabel, stripLabel )
import Scheduler ( SchedulerContext(..), Event, UserId, SessionId, workspaceToMessage, eventMessage, renumberEvent )
import Util ( Counter, newCounter )
import Time ( Time(..), LogicalTime )
import Workspace ( Workspace(..), WorkspaceId, VersionId )

data CacheState = CacheState {
    functionCounter :: Counter,
    workspacesC :: TVar (M.Map VersionId Workspace),
    -- messagesC :: ,
    answersC :: TVar (M.Map VersionId Message),
    answerFunctionsC :: TVar (M.Map SessionId FunctionId),
    pointersC :: TVar (M.Map Pointer Message),
    alternativesC :: TVar (M.Map FunctionId [([Pattern], Exp')]),
    linksC :: TVar (M.Map VersionId PointerRemapping),
    continuationsC :: TVar (M.Map VersionId (M.Map FunctionId Konts')),
    continuationArgumentsC :: TVar (M.Map (VersionId, FunctionId) (M.Map Int Message)),
    traceC :: TVar [(ProcessId, EvalState')], -- Stored newest first.
    runQueueC :: TVar (M.Map SessionId (M.Map ProcessId EvalState')),
    sessionsC :: TVar (M.Map SessionId [ProcessId]) }

createCache :: Snapshot -> IO CacheState
createCache snapshot = do
    functionC <- newCounter (functionCounterS snapshot)
    workspacesTVar <- newTVarIO (workspacesS snapshot)
    answersTVar <- newTVarIO (answersS snapshot)
    answerFunctionsTVar <- newTVarIO (answerFunctionsS snapshot)
    pointersTVar <- newTVarIO (pointersS snapshot)
    alternativesTVar <- newTVarIO (alternativesS snapshot)
    linksTVar <- newTVarIO (linksS snapshot)
    continuationsTVar <- newTVarIO (continuationsS snapshot)
    continuationArgumentsTVar <- newTVarIO (continuationArgumentsS snapshot)
    traceTVar <- newTVarIO (traceS snapshot)
    runQueueTVar <- newTVarIO (runQueueS snapshot)
    sessionsTVar <- newTVarIO (sessionsS snapshot)

    return $ CacheState {
                functionCounter = functionC,
                workspacesC = workspacesTVar,
                answersC = answersTVar,
                answerFunctionsC = answerFunctionsTVar,
                pointersC = pointersTVar,
                alternativesC = alternativesTVar,
                linksC = linksTVar,
                continuationsC = continuationsTVar,
                continuationArgumentsC = continuationArgumentsTVar,
                traceC = traceTVar,
                runQueueC = runQueueTVar,
                sessionsC = sessionsTVar
             }

makeCachingSchedulerContext :: CacheState -> SchedulerContext e -> IO (SchedulerContext e)
makeCachingSchedulerContext cache ctxt = do
    return $ SchedulerContext {
                doAtomically = id, -- doAtomically ctxt, -- TODO: XXX Think about this.
                createInitialWorkspace = createInitialWorkspaceCaching cache ctxt,
                newSession = newSessionCaching cache ctxt,
                newVersion = newVersionCaching cache ctxt,
                createWorkspace = createWorkspaceCaching cache ctxt,
                sendAnswer = sendAnswerCaching cache ctxt,
                sendMessage = sendMessageCaching cache ctxt,
                expandPointer = expandPointerCaching cache ctxt,
                nextPointer = nextPointerCaching cache ctxt,
                createPointers = createPointersCaching cache ctxt,
                remapPointers = remapPointersCaching cache ctxt,
                pendingQuestions = pendingQuestionsCaching cache ctxt,
                getWorkspace = getWorkspaceCaching cache ctxt,
                workspaceIdOf = workspaceIdOfCaching cache ctxt,
                getNextWorkspace = getNextWorkspaceCaching cache ctxt,
                dereference = dereferenceCaching cache ctxt,
                reifyWorkspace = reifyWorkspaceCaching cache ctxt,
                extraContent = extraContent ctxt
            }

-- Backend calls:
--      getWorkspace - Synchronous but only used in createInitialWorkspace
--      createInitialWorkspace - Asynchronous
--      newSession - Asynchronous
--      createWorkspace - Asynchronous
--      sendAnswer - Asynchronous
--      expandPointer - Asynchronous
--      createPointers - Asynchronous
--      remapPointers - Asynchronous

reifyWorkspaceCaching :: CacheState -> SchedulerContext e -> VersionId -> IO Message
reifyWorkspaceCaching cache ctxt versionId = do
    workspaces <- readTVarIO (workspacesC cache)
    return (workspaceToMessage workspaces versionId)

dereferenceCaching :: CacheState -> SchedulerContext e -> Pointer -> IO Message
dereferenceCaching cache ctxt ptr = do
    mVal <- M.lookup ptr <$> readTVarIO (pointersC cache)
    case mVal of
        Just msg -> return $! applyLabel ptr msg
        -- Intentionally incomplete.

createInitialWorkspaceCaching :: CacheState -> SchedulerContext e -> Message -> IO VersionId
createInitialWorkspaceCaching cache ctxt msg = do
    wsId <- createInitialWorkspace ctxt msg
    ws <- getWorkspace ctxt wsId
    atomically $ modifyTVar' (workspacesC cache) (M.insert wsId ws)
    return wsId

newSessionCaching :: CacheState -> SchedulerContext e -> Maybe SessionId -> IO SessionId
newSessionCaching cache ctxt mSessionId = do
    sessionId <- newSession ctxt mSessionId
    atomically $ modifyTVar' (sessionsC cache) (M.unionWith (++) (M.singleton sessionId []))
    return sessionId

newVersionCaching :: CacheState -> SchedulerContext e -> VersionId -> IO (VersionId, LogicalTime)
newVersionCaching cache ctxt versionId = newVersion ctxt versionId -- TODO: Anything else necessary?

createWorkspaceCaching :: CacheState -> SchedulerContext e -> UserId -> VersionId -> Message -> Message -> IO (VersionId, WorkspaceId, LogicalTime)
createWorkspaceCaching cache ctxt userId versionId qAsAsked qAsAnswered = do
    r@(childVId, childId, t) <- createWorkspace ctxt userId versionId qAsAsked qAsAnswered
    let newChildWorkspace = Workspace {
                             identity = childVId,
                             workspaceIdentity = childId,
                             parentId = Just versionId,
                             previousVersion = Nothing,
                             question = qAsAnswered,
                             subQuestions = [],
                             messageHistory = [],
                             expandedPointers = M.empty,
                             time = Time t }
        new ws = ws {
                    subQuestions = subQuestions ws ++ [(childVId, qAsAsked, Nothing)],
                    time = Time t }

    atomically $ modifyTVar' (workspacesC cache) (M.insert childVId newChildWorkspace . M.adjust new versionId)
    return r

sendAnswerCaching :: CacheState -> SchedulerContext e -> UserId -> VersionId -> Message -> IO ()
sendAnswerCaching cache ctxt userId versionId msg = do
    sendAnswer ctxt userId versionId msg
    Workspace { parentId = mParentWorkspaceId } <- getWorkspaceCaching cache ctxt versionId
    case mParentWorkspaceId of
        Nothing -> atomically $ modifyTVar' (answersC cache) (M.insert versionId msg)
        Just parentWorkspaceVersionId -> atomically $ do
            let new ws = ws { subQuestions = map addAnswer (subQuestions ws) }
            modifyTVar' (workspacesC cache) (M.adjust new versionId)
            modifyTVar' (answersC cache) (M.insert versionId msg)
  where addAnswer sq@(qId, q, _) | qId == versionId = (qId, q, Just msg)
                                 | otherwise = sq

sendMessageCaching :: CacheState -> SchedulerContext e -> UserId -> VersionId -> VersionId -> Message -> IO ()
sendMessageCaching cache ctxt userId srcId tgtId msg = error "sendMessageCaching: not implemented"

expandPointerCaching :: CacheState -> SchedulerContext e -> UserId -> VersionId -> Pointer -> IO ()
expandPointerCaching cache ctxt userId versionId ptr = do
    expandPointer ctxt userId versionId ptr
    atomically $ do
        msg <- applyLabel ptr . (\m -> case M.lookup ptr m of Just a -> a) <$> readTVar (pointersC cache)
        let new ws = ws { expandedPointers = M.insert ptr msg (expandedPointers ws) }
        modifyTVar' (workspacesC cache) (M.adjust new versionId)

nextPointerCaching :: CacheState -> SchedulerContext e -> IO Pointer
nextPointerCaching cache ctxt = do
    ps <- readTVarIO (pointersC cache)
    return $! maybe 0 (succ . fst) (M.lookupMax ps)

createPointersCaching :: CacheState -> SchedulerContext e -> PointerEnvironment -> IO ()
createPointersCaching cache ctxt env = do
    let !env' = fmap stripLabel env
    atomically $ modifyTVar' (pointersC cache) (M.union env')
    createPointers ctxt env

remapPointersCaching :: CacheState -> SchedulerContext e -> PointerRemapping -> IO ()
remapPointersCaching cache ctxt mapping = do
    atomically $ modifyTVar' (pointersC cache) (\ps -> M.union (fmap (ps M.!) mapping) ps)
    remapPointers ctxt mapping

pendingQuestionsCaching :: CacheState -> SchedulerContext e -> VersionId -> IO [VersionId]
pendingQuestionsCaching cache ctxt versionId = do
    ws <- (\m -> case M.lookup versionId m of Just a -> a) <$> readTVarIO (workspacesC cache)
    return $ mapMaybe (\(wsId, _, ma) -> maybe (Just wsId) (const Nothing) ma) (subQuestions ws)

getWorkspaceCaching :: CacheState -> SchedulerContext e -> VersionId -> IO Workspace
getWorkspaceCaching cache ctxt versionId = do
    (\m -> case M.lookup versionId m of Just a -> a) <$> readTVarIO (workspacesC cache)

workspaceIdOfCaching :: CacheState -> SchedulerContext e -> VersionId -> IO WorkspaceId
workspaceIdOfCaching cache ctxt versionId = workspaceIdentity <$> getWorkspaceCaching cache ctxt versionId

getNextWorkspaceCaching :: CacheState -> SchedulerContext e -> IO (Maybe VersionId)
getNextWorkspaceCaching cache ctxt = error "getNextWorkspaceCaching: not implemented"
