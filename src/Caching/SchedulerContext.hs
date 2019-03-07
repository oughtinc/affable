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
import Workspace ( Workspace(..), WorkspaceId )

data CacheState = CacheState {
    functionCounter :: Counter,
    workspacesC :: TVar (M.Map WorkspaceId Workspace),
    -- messagesC :: ,
    answersC :: TVar (M.Map WorkspaceId Message),
    answerFunctionsC :: TVar (M.Map SessionId FunctionId),
    pointersC :: TVar (M.Map Pointer Message),
    alternativesC :: TVar (M.Map FunctionId [([Pattern], Exp')]),
    linksC :: TVar (M.Map WorkspaceId PointerRemapping),
    continuationsC :: TVar (M.Map WorkspaceId (M.Map FunctionId Konts')),
    continuationArgumentsC :: TVar (M.Map (WorkspaceId, FunctionId) (M.Map Int Message)),
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
                createWorkspace = createWorkspaceCaching cache ctxt,
                sendAnswer = sendAnswerCaching cache ctxt,
                sendMessage = sendMessageCaching cache ctxt,
                expandPointer = expandPointerCaching cache ctxt,
                nextPointer = nextPointerCaching cache ctxt,
                createPointers = createPointersCaching cache ctxt,
                remapPointers = remapPointersCaching cache ctxt,
                pendingQuestions = pendingQuestionsCaching cache ctxt,
                getWorkspace = getWorkspaceCaching cache ctxt,
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

reifyWorkspaceCaching :: CacheState -> SchedulerContext e -> WorkspaceId -> IO Message
reifyWorkspaceCaching cache ctxt workspaceId = do
    workspaces <- readTVarIO (workspacesC cache)
    return (workspaceToMessage workspaces workspaceId)

dereferenceCaching :: CacheState -> SchedulerContext e -> Pointer -> IO Message
dereferenceCaching cache ctxt ptr = do
    mVal <- M.lookup ptr <$> readTVarIO (pointersC cache)
    case mVal of
        Just msg -> return $! applyLabel ptr msg
        -- Intentionally incomplete.

createInitialWorkspaceCaching :: CacheState -> SchedulerContext e -> Message -> IO WorkspaceId
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

createWorkspaceCaching :: CacheState -> SchedulerContext e -> UserId -> WorkspaceId -> Message -> Message -> IO WorkspaceId
createWorkspaceCaching cache ctxt userId workspaceId qAsAsked qAsAnswered = do
    wsId <- createWorkspace ctxt userId workspaceId qAsAsked qAsAnswered
    let newWorkspace = Workspace {
                        identity = wsId,
                        parentId = Just workspaceId,
                        question = qAsAnswered,
                        subQuestions = [],
                        messageHistory = [],
                        expandedPointers = M.empty,
                        time = 0 }
    atomically $ modifyTVar' (workspacesC cache) (M.insert wsId newWorkspace . M.adjust (insertSubQuestion wsId) workspaceId)
    return wsId
  where insertSubQuestion wsId ws@(Workspace { subQuestions = sqs }) = ws { subQuestions = sqs ++ [(wsId, qAsAsked, Nothing)] }

sendAnswerCaching :: CacheState -> SchedulerContext e -> UserId -> WorkspaceId -> Message -> IO ()
sendAnswerCaching cache ctxt userId workspaceId msg = do
    Workspace { parentId = mParentWorkspaceId } <- getWorkspaceCaching cache ctxt workspaceId
    case mParentWorkspaceId of
        Nothing -> atomically $ modifyTVar' (answersC cache) (M.insert workspaceId msg)
        Just parentWorkspaceId -> do
            atomically $ do
                modifyTVar' (workspacesC cache) (M.adjust (insertSubQuestion workspaceId msg) parentWorkspaceId)
                modifyTVar' (answersC cache) (M.insert workspaceId msg)
    sendAnswer ctxt userId workspaceId msg
  where insertSubQuestion wsId msg ws@(Workspace { subQuestions = sqs }) = ws { subQuestions = map addAnswer sqs }
            where addAnswer sq@(qId, q, _) | qId == wsId = (qId, q, Just msg)
                                           | otherwise = sq

sendMessageCaching :: CacheState -> SchedulerContext e -> UserId -> WorkspaceId -> WorkspaceId -> Message -> IO ()
sendMessageCaching cache ctxt userId srcId tgtId msg = error "sendMessageCaching: not implemented"

expandPointerCaching :: CacheState -> SchedulerContext e -> UserId -> WorkspaceId -> Pointer -> IO ()
expandPointerCaching cache ctxt userId workspaceId ptr = do
    atomically $ do
        msg <- applyLabel ptr . (\m -> case M.lookup ptr m of Just a -> a) <$> readTVar (pointersC cache)
        modifyTVar' (workspacesC cache) $ M.adjust (insertPointer msg) workspaceId
    expandPointer ctxt userId workspaceId ptr
  where insertPointer msg ws@(Workspace { expandedPointers = ep }) = ws { expandedPointers = M.insert ptr msg ep }

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

pendingQuestionsCaching :: CacheState -> SchedulerContext e -> WorkspaceId -> IO [WorkspaceId]
pendingQuestionsCaching cache ctxt workspaceId = do
    ws <- (\m -> case M.lookup workspaceId m of Just a -> a) <$> readTVarIO (workspacesC cache)
    return $ mapMaybe (\(wsId, _, ma) -> maybe (Just wsId) (const Nothing) ma) (subQuestions ws)

getWorkspaceCaching :: CacheState -> SchedulerContext e -> WorkspaceId -> IO Workspace
getWorkspaceCaching cache ctxt workspaceId = do
    (\m -> case M.lookup workspaceId m of Just a -> a) <$> readTVarIO (workspacesC cache)

getNextWorkspaceCaching :: CacheState -> SchedulerContext e -> IO (Maybe WorkspaceId)
getNextWorkspaceCaching cache ctxt = error "getNextWorkspaceCaching: not implemented"
