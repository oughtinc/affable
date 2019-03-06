{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module Caching.SchedulerContext ( CacheState(..), makeCachingSchedulerContext ) where
import Control.Concurrent.STM ( TVar, atomically, newTVarIO, readTVar, readTVarIO, writeTVar, modifyTVar' ) -- stm
import qualified Data.Map as M -- containers
import Data.Maybe ( mapMaybe ) -- base
import Data.Traversable ( mapAccumL ) -- base

import AutoScheduler ( FunctionId, ProcessId )
import Exp ( Pattern, Exp', Konts', EvalState' )
import Message ( Message(..), Pointer, PointerEnvironment, PointerRemapping, normalizeMessage, generalizeMessage,
                 canonicalizeMessage, boundPointers )
import Scheduler ( SchedulerContext(..), Event, UserId, SessionId, workspaceToMessage, eventMessage, renumberEvent )
import Time ( Time(..), LogicalTime )
import Workspace ( Workspace(..), WorkspaceId )

data CacheState = CacheState {
    workspacesC :: TVar (M.Map WorkspaceId Workspace),
    -- messages :: ,
    answersC :: TVar (M.Map WorkspaceId Message),
    answerFunctionsC :: TVar (M.Map SessionId FunctionId),
    pointersC :: TVar (M.Map Pointer Message),
    alternativesC :: TVar (M.Map FunctionId [([Pattern], Exp')]),
    linksC :: TVar (M.Map WorkspaceId PointerRemapping),
    continuationsC :: TVar (M.Map WorkspaceId (M.Map FunctionId Konts')),
    continuationArgumentsC :: TVar (M.Map (WorkspaceId, FunctionId) (M.Map Int Message)),
    traceC :: TVar [(ProcessId, EvalState')], -- Stored newest first.
    runQueueC :: TVar (M.Map ProcessId EvalState'),
    sessionsC :: TVar (M.Map SessionId [ProcessId]) }

createCache :: SchedulerContext e -> IO CacheState
createCache ctxt = do -- TODO: Somehow pull state from database.
    workspacesTVar <- newTVarIO M.empty
    answersTVar <- newTVarIO M.empty
    answerFunctionsTVar <- newTVarIO M.empty
    pointersTVar <- newTVarIO M.empty
    alternativesTVar <- newTVarIO M.empty
    linksTVar <- newTVarIO M.empty
    continuationsTVar <- newTVarIO M.empty
    continuationArgumentsTVar <- newTVarIO M.empty
    traceTVar <- newTVarIO []
    runQueueTVar <- newTVarIO M.empty
    sessionsTVar <- newTVarIO M.empty

    return $ CacheState {
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

makeCachingSchedulerContext :: SchedulerContext e -> IO (CacheState, SchedulerContext e)
makeCachingSchedulerContext ctxt = do
    cache <- createCache ctxt
    return (cache,
        SchedulerContext {
            createInitialWorkspace = createInitialWorkspaceCaching cache ctxt,
            newSession = newSessionCaching cache ctxt,
            createWorkspace = createWorkspaceCaching cache ctxt,
            sendAnswer = sendAnswerCaching cache ctxt,
            sendMessage = sendMessageCaching cache ctxt,
            expandPointer = expandPointerCaching cache ctxt,
            createPointers = createPointersCaching cache ctxt,
            pendingQuestions = pendingQuestionsCaching cache ctxt,
            getWorkspace = getWorkspaceCaching cache ctxt,
            allWorkspaces = allWorkspacesCaching cache ctxt,
            getNextWorkspace = getNextWorkspaceCaching cache ctxt,
            labelMessage = labelMessageCaching cache ctxt,
            normalizeEnv = insertMessagePointers cache ctxt,
            canonicalizeEventsEnv = canonicalizeEventsCaching cache ctxt,
            generalizeEnv = insertGeneralizedMessagePointers cache ctxt,
            dereference = dereferenceCaching cache ctxt,
            reifyWorkspace = reifyWorkspaceCaching cache ctxt,
            extraContent = extraContent ctxt
        })

reifyWorkspaceCaching :: CacheState -> SchedulerContext e -> WorkspaceId -> IO Message
reifyWorkspaceCaching cache ctxt workspaceId = do
    workspaces <- readTVarIO (workspacesC cache)
    return (workspaceToMessage workspaces workspaceId)

dereferenceCaching :: CacheState -> SchedulerContext e -> Pointer -> IO Message
dereferenceCaching cache ctxt ptr = do
    mVal <- M.lookup ptr <$> readTVarIO (pointersC cache)
    case mVal of
        Just msg -> return msg
        -- Intentionally incomplete.

canonicalizeEventsCaching :: CacheState -> SchedulerContext e -> [Event] -> IO (PointerEnvironment, [Event])
canonicalizeEventsCaching cache ctxt evts = do
    r@(pEnv, _) <- canonicalizeEventsEnv ctxt evts
    atomically $ modifyTVar' (pointersC cache) (M.union pEnv)
    return r

insertMessagePointers :: CacheState -> SchedulerContext e -> Message -> IO (PointerEnvironment, Message)
insertMessagePointers cache ctxt msg = do
    r@(pEnv, _) <- normalizeEnv ctxt msg
    atomically $ modifyTVar' (pointersC cache) (M.union pEnv)
    return r

insertGeneralizedMessagePointers :: CacheState -> SchedulerContext e -> Message -> IO (PointerRemapping, Message)
insertGeneralizedMessagePointers cache ctxt msg = do
    r@(mapping, _) <- generalizeEnv ctxt msg
    atomically $ modifyTVar' (pointersC cache) (\pointers -> M.union (fmap (pointers M.!) mapping) pointers) -- TODO: Better error handling.
    return r

labelMessageCaching :: CacheState -> SchedulerContext e -> Message -> IO Message
labelMessageCaching cache ctxt msg = do
    msg@(LabeledStructured p _) <- labelMessage ctxt msg
    atomically $ modifyTVar' (pointersC cache) (M.insert p msg)
    return msg

createInitialWorkspaceCaching :: CacheState -> SchedulerContext e -> IO WorkspaceId
createInitialWorkspaceCaching cache ctxt = do
    wsId <- createInitialWorkspace ctxt
    ws <- getWorkspace ctxt wsId
    atomically $ modifyTVar' (workspacesC cache) (M.insert wsId ws)
    return wsId

newSessionCaching :: CacheState -> SchedulerContext e -> Maybe SessionId -> IO SessionId
newSessionCaching cache ctxt mSessionId = do
    sessionId <- newSession ctxt mSessionId
    atomically $ modifyTVar' (sessionsC cache) (M.unionWith (++) (M.singleton sessionId []))
    return sessionId

createWorkspaceCaching :: CacheState -> SchedulerContext e -> Bool -> UserId -> WorkspaceId -> Message -> Message -> IO WorkspaceId
createWorkspaceCaching cache ctxt doNormalize userId workspaceId qAsAsked qAsAnswered = do
    qAsAnswered' <- if doNormalize then snd <$> insertMessagePointers cache ctxt qAsAnswered else return qAsAnswered
    wsId <- createWorkspace ctxt False userId workspaceId qAsAsked qAsAnswered'
    let newWorkspace = Workspace {
                        identity = wsId,
                        parentId = Just workspaceId,
                        question = qAsAnswered',
                        subQuestions = [],
                        messageHistory = [],
                        expandedPointers = M.empty,
                        time = 0 }
    atomically $ modifyTVar' (workspacesC cache) (M.insert wsId newWorkspace . M.adjust (insertSubQuestion wsId) workspaceId)
    return wsId
  where insertSubQuestion wsId ws@(Workspace { subQuestions = sqs }) = ws { subQuestions = sqs ++ [(wsId, qAsAsked, Nothing)] }

sendAnswerCaching :: CacheState -> SchedulerContext e -> Bool -> UserId -> WorkspaceId -> Message -> IO ()
sendAnswerCaching cache ctxt doNormalize userId workspaceId msg = do
    msg' <- if doNormalize then snd <$> insertMessagePointers cache ctxt msg else return msg
    Workspace { parentId = mParentWorkspaceId } <- getWorkspaceCaching cache ctxt workspaceId
    case mParentWorkspaceId of
        Nothing -> atomically $ modifyTVar' (answersC cache) (M.insert workspaceId msg')
        Just parentWorkspaceId -> do
            atomically $ do
                modifyTVar' (workspacesC cache) (M.adjust (insertSubQuestion workspaceId msg') parentWorkspaceId)
                modifyTVar' (answersC cache) (M.insert workspaceId msg')
    sendAnswer ctxt False userId workspaceId msg'
  where insertSubQuestion wsId msg ws@(Workspace { subQuestions = sqs }) = ws { subQuestions = map addAnswer sqs }
            where addAnswer sq@(qId, q, _) | qId == wsId = (qId, q, Just msg)
                                           | otherwise = sq

sendMessageCaching :: CacheState -> SchedulerContext e -> Bool -> UserId -> WorkspaceId -> WorkspaceId -> Message -> IO ()
sendMessageCaching cache ctxt doNormalize userId srcId tgtId msg = error "sendMessageCaching: not implemented"

expandPointerCaching :: CacheState -> SchedulerContext e -> UserId -> WorkspaceId -> Pointer -> IO ()
expandPointerCaching cache ctxt userId workspaceId ptr = do
    atomically $ do
        msg <- (\m -> case M.lookup ptr m of Just a -> a) <$> readTVar (pointersC cache)
        modifyTVar' (workspacesC cache) $ M.adjust (insertPointer msg) workspaceId
    expandPointer ctxt userId workspaceId ptr
  where insertPointer msg ws@(Workspace { expandedPointers = ep }) = ws { expandedPointers = M.insert ptr msg ep }

createPointersCaching :: CacheState -> SchedulerContext e -> PointerEnvironment -> IO ()
createPointersCaching cache ctxt env = do
    atomically $ modifyTVar' (pointersC cache) (M.union env)
    createPointers ctxt env

pendingQuestionsCaching :: CacheState -> SchedulerContext e -> WorkspaceId -> IO [WorkspaceId]
pendingQuestionsCaching cache ctxt workspaceId = do
    ws <- (\m -> case M.lookup workspaceId m of Just a -> a) <$> readTVarIO (workspacesC cache)
    return $ mapMaybe (\(wsId, _, ma) -> maybe (Just wsId) (const Nothing) ma) (subQuestions ws)

getWorkspaceCaching :: CacheState -> SchedulerContext e -> WorkspaceId -> IO Workspace
getWorkspaceCaching cache ctxt workspaceId = do
    (\m -> case M.lookup workspaceId m of Just a -> a) <$> readTVarIO (workspacesC cache)

allWorkspacesCaching :: CacheState -> SchedulerContext e -> IO (M.Map WorkspaceId Workspace)
allWorkspacesCaching cache ctxt = readTVarIO (workspacesC cache)

getNextWorkspaceCaching :: CacheState -> SchedulerContext e -> IO (Maybe WorkspaceId)
getNextWorkspaceCaching cache ctxt = error "getNextWorkspaceCaching: not implemented"
