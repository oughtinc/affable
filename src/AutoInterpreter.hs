{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module AutoInterpreter where
import Control.Concurrent ( ThreadId, forkIO ) -- base
import Control.Concurrent.Async ( mapConcurrently ) -- async
-- import Control.Concurrent.Async.Pool ( withTaskGroup, mapTasks ) -- async-pool
import Control.Concurrent.MVar ( MVar, newEmptyMVar, newMVar, putMVar, takeMVar, readMVar, modifyMVar_ ) -- base
import Control.Concurrent.Chan ( Chan, newChan, readChan, writeChan ) -- base TODO: Use TChan instead?
import Control.Exception ( bracket_ ) -- base
import Control.Monad ( zipWithM, zipWithM_ ) -- base
import Data.Foldable ( asum, forM_ ) -- base
import Data.IORef ( IORef, newIORef, readIORef, writeIORef, atomicModifyIORef', modifyIORef' ) -- base
import qualified Data.Map as M -- containers
import Data.Maybe ( mapMaybe ) -- base
import Data.String ( fromString ) -- base
import Data.Text ( Text ) -- text
import qualified Data.Text as T -- text
import qualified Data.Text.IO as T -- text
import Data.Text.Lazy.Builder ( Builder, singleton, fromText ) -- text
import Data.Traversable ( traverse ) -- base
import Data.Tuple ( swap ) -- base
import System.IO ( stderr) -- base

import AutoScheduler ( AutoSchedulerContext(..) )
import Exp ( Exp(..), Exp', Name(..), VarEnv, Var, Value, Primitive, Pattern, evaluateExp, expToBuilder, expToHaskell )
import Message ( Message(..), Pointer, PointerRemapping, messageToBuilder, matchMessage,
                 matchPointers, expandPointers, substitute, renumberMessage' )
import Primitive ( makePrimitives )
import Scheduler ( Event(..), SchedulerContext(..), SchedulerFn, relabelMessage, fullyExpand )
import Util ( toText, invertMap )
import Workspace ( WorkspaceId, Workspace(..), emptyWorkspace )

-- NOTE: Instead of using forkIO and co, we could use a monad other than IO for
-- expression evaluation that supports suspending a computation or implements cooperative
-- concurrency.

type MatchFn = WorkspaceId -> VarEnv Var -> Name -> [Message] -> IO (WorkspaceId, VarEnv Var, Exp')

makeMatcher :: (Bool -> WorkspaceId -> IO Event)
            -> (Value -> Maybe Primitive)
            -> (WorkspaceId -> Message -> IO())
            -> (WorkspaceId -> IO (Maybe Message))
            -> AutoSchedulerContext extra -> IO MatchFn
makeMatcher blockOnUser matchPrim giveArgument retrieveArgument autoCtxt = do
    let !ctxt = schedulerContext autoCtxt

    -- TODO: This could probably be made unnecessary if we started tracking timing, i.e. using that logicalTime field.
    cutOffsRef <- newIORef (M.empty :: M.Map WorkspaceId WorkspaceId)

    -- Get answers that have been answered since last time the workspace was displayed.
    let getRecentlyAnswered workspace = do
            let !workspaceId = identity workspace
                !answeredMax = case mapMaybe (\(qId, _, ma) -> qId <$ ma) $ subQuestions workspace of [] -> -1; qIds -> maximum qIds
            currCutOff <- maybe (-1) id <$> atomicModifyIORef' cutOffsRef (swap . M.insertLookupWithKey (\_ new _ -> new) workspaceId answeredMax)
            -- Answers of workspaces that have answers and are newer than the cutoff.
            return $! mapMaybe (\(qId, _, ma) -> if qId > currCutOff then ma else Nothing) $ subQuestions workspace

    -- Mapping of pointers from replay workspace to original workspace.
    workspaceVariablesRef <- newIORef (M.empty :: M.Map WorkspaceId PointerRemapping) -- TODO: Store in database(?) Maybe not?

    let linkVars workspaceId mapping = modifyIORef' workspaceVariablesRef $ M.insertWith M.union workspaceId mapping
        links workspaceId = (maybe M.empty id . M.lookup workspaceId) <$> readIORef workspaceVariablesRef

        -- If retrieveArgument produces something then we've recently done a variable lookup and need
        -- to map pointers from it. Otherwise we just map pointers from all the answers that have been
        -- added in the latest batch.
        linkPointers ANSWER workspace [pattern] = do
            let !workspaceId = identity workspace
            linkVars workspaceId $ matchPointers pattern $ question workspace
        linkPointers _ workspace patterns = do
            let !workspaceId = identity workspace
            mDeref <- retrieveArgument workspaceId
            case (mDeref, patterns) of
                (Just a, [pattern]) -> linkVars workspaceId $ matchPointers pattern a
                (Nothing, _) -> do
                    recentlyAnswered <- getRecentlyAnswered workspace
                    zipWithM_ (\pattern a -> linkVars workspaceId $ matchPointers pattern a) patterns recentlyAnswered
                -- Intentionally incomplete.

        debugCode = do
            altMap <- allAlternatives autoCtxt
            -- T.hPutStrLn stderr (toText (expToHaskell (\f -> maybe [] reverse $ M.lookup f altMap) (LetFun ANSWER (Value (Text "dummy")))))
            T.hPutStrLn stderr (toText (expToBuilder (\f -> maybe [] reverse $ M.lookup f altMap) (LetFun ANSWER (Value (Text "dummy")))))

    -- Store matches that are being worked on. Definitely does NOT need to be in the database.
    pendingMatchesRef <- newIORef (M.empty :: M.Map Name (MVar [([Pattern], MVar ())]))

    -- When we receive the Submit event, we look at the unanswered questions of the current workspace for the parameters.
    -- Expand doesn't get batched, so we only have batches of questions, i.e. multi-argument function calls are always
    -- of the form `f(answer(...), prim1(...), ...)`.
    let match s varEnv f [Reference p] = do
            error "Did you get here? If so, tell me how."
            m <- dereference ctxt p
            match s varEnv f [m]
        match workspaceId varEnv f ms = do
            ms' <- mapM (\m -> normalize ctxt =<< generalize ctxt m) ms

            -- Atomically:
            --  - Match against each pending pattern.
            --  - If there is a match then return the corresponding lock. DON'T block on the lock here. Only after we finish processing the map.
            --  - If there isn't a match, create and insert a lock for a pattern corresponding to the current message.
            -- If we found an existing lock, then block on it and execute retryLoop when it releases.
            -- If we made a new lock, then continue with matchFailed and release the lock after we addCaseFor or if we're killed.
            --  - We also need to clean out the mapping in this case before we release the lock.
            --  - We should be able to use bracket_ to do this with a trivial acquire function as the lock should be created already locked.
            let matchPending workspace = do
                    pendingMatchesMVar <- do
                        fMVar <- newMVar [] -- This will get garbage collected if we don't use it, which we won't except for the first time.
                        mMVar <- atomicModifyIORef' pendingMatchesRef $ swap . M.insertLookupWithKey (\_ _ old -> old) f fMVar
                        return $ maybe fMVar id mMVar

                    -- Begin atomic block
                    pendingMatches <- takeMVar pendingMatchesMVar
                    -- TODO: Error out if there is more than one match.
                    case asum $ map (\(ps, pMVar) -> pMVar <$ zipWithM matchMessage ps ms') pendingMatches of
                        Just pMVar -> do
                            putMVar pendingMatchesMVar pendingMatches
                            -- End atomic block
                            readMVar pMVar -- block until filled
                            retryLoop
                        Nothing -> do
                            pMVar <- newEmptyMVar -- locked lock
                            putMVar pendingMatchesMVar ((ms', pMVar):pendingMatches)
                            -- End atomic block
                            bracket_ (return ())
                                     (do modifyMVar_ pendingMatchesMVar (return . filter ((pMVar/=) . snd)); putMVar pMVar ())
                                     (matchFailed workspace ms')

                retryLoop = do
                    workspace <- getWorkspace ctxt workspaceId
                    alts <- alternativesFor autoCtxt f
                    case alts of -- TODO: Could mark workspaces as "human-influenced" when a pattern match failure is hit
                                 -- or when any subquestions are marked. This would allow "garbage collecting" workspaces
                                 -- with answers that are not "human-influenced", i.e. were created entirely through automation.
                        _:_ -> do
                            -- TODO: Error out if there is more than one match.
                            let !mMatch = asum $ map (\(ps, e) -> fmap (\bindings -> (ps, M.union (M.unions bindings) varEnv, e)) $ zipWithM matchMessage ps ms') alts
                            case mMatch of
                                Just (patterns, varEnv', e) -> do
                                    varEnv' <- traverse (\case Reference p -> dereference ctxt p; x -> return x) varEnv'

                                    -- This is to make it so occurrences of variables bound by as-patterns don't get substituted.
                                    let bindings = M.union (M.unions $
                                                                zipWith (\p m -> case (p, m) of
                                                                                    (LabeledStructured asP _, LabeledStructured l _) -> M.singleton asP (Reference l)
                                                                                    _ -> M.empty)
                                                                      patterns ms') varEnv'

                                    linkPointers f workspace patterns
                                    globalToLocal <- links workspaceId

                                    -- This is a bit hacky. If this approach is the way to go, make these patterns individual constructors.
                                    case e of
                                        LetFun _ (Call _ [Var ptr]) -> do -- expand case
                                            let !p = maybe ptr id $ M.lookup ptr $ invertMap globalToLocal
                                            arg <- dereference ctxt p
                                            expandPointer ctxt workspaceId p
                                            giveArgument workspaceId arg
                                            return (workspaceId, M.insert ptr arg varEnv', e)
                                        LetFun _ (Call _ args) -> do -- ask cases
                                            let !localToGlobal = invertMap globalToLocal
                                            forM_ args $ \argExp -> do
                                                let !m = case argExp of Call ANSWER [Value m] -> m; Prim _ (Value m) -> m
                                                let !arg = substitute bindings m
                                                pattern <- relabelMessage ctxt =<< normalize ctxt =<< generalize ctxt arg
                                                createWorkspace ctxt False workspaceId (renumberMessage' localToGlobal m) pattern
                                            return (workspaceId, varEnv', e)
                                        Value msg -> do -- reply case
                                            let !msg' = substitute bindings msg -- TODO: Need to do the same local-to-global renumbering as above?
                                            msg <- normalize ctxt =<< case msg' of Reference p -> dereference ctxt p; _ -> relabelMessage ctxt msg'
                                            sendAnswer ctxt False workspaceId msg -- TODO: Can I clean out workspaceVariablesRef a bit now?
                                            return (workspaceId, varEnv', e)
                                        -- _ -> return (workspaceId, varEnv', e) -- Intentionally missing this case.
                                Nothing -> matchPending workspace
                        [] -> matchPending workspace
            retryLoop
          where matchFailed workspace ms' = do
                    let !workspaceId = identity workspace
                    patterns <- mapM (relabelMessage ctxt) ms'
                    let !(Just bindings) = M.unions <$> zipWithM matchMessage patterns ms' -- This shouldn't fail.
                    bindings <- traverse (\case Reference p -> dereference ctxt p; x -> return x) bindings
                    let !varEnv' = M.union bindings varEnv

                    linkPointers f workspace patterns
                    globalToLocal <- links workspaceId

                    let loop stay = blockOnUser stay workspaceId >>= processEvent
                        processEvent (Create msg) = do
                            pattern <- relabelMessage ctxt =<< normalize ctxt =<< generalize ctxt msg
                            createWorkspace ctxt False workspaceId msg pattern
                            loop True
                        processEvent (Expand ptr) = do -- TODO: Make this more resilient to pointers that are not in scope.
                            expandPointer ctxt workspaceId ptr
                            arg <- dereference ctxt ptr
                            giveArgument workspaceId arg
                            g <- newFunction autoCtxt
                            let !ptr' = maybe ptr id $ M.lookup ptr globalToLocal
                            return (M.singleton ptr' arg, LetFun g (Call g [Var ptr']))
                        processEvent (Answer msg@(Structured [Reference p])) = do -- dereference pointers -- TODO: Do this?
                            msg' <- dereference ctxt p
                            sendAnswer ctxt False workspaceId msg' -- TODO: Can I clean out workspaceVariablesRef a bit now?
                            return (M.empty, Value $ renumberMessage' globalToLocal msg)
                        processEvent (Answer msg) = do
                            msg' <- relabelMessage ctxt =<< normalize ctxt msg
                            sendAnswer ctxt False workspaceId msg' -- TODO: Can I clean out workspaceVariablesRef a bit now?
                            return (M.empty, Value $ renumberMessage' globalToLocal msg)
                        processEvent Submit = do
                            workspace <- getWorkspace ctxt workspaceId -- Refresh workspace.
                            -- Get unanswered questions.
                            let !qs = mapMaybe (\(_, q, ma) -> maybe (Just q) (\_ -> Nothing) ma) $ subQuestions workspace
                            if null qs then loop False else do -- If qs is empty there's nothing to wait on so just do nothing.
                                g <- newFunction autoCtxt
                                let args = map (\q -> primToCall (matchPrim q) q) qs
                                return (M.empty, LetFun g (Call g args))
                        -- processEvent (Send ws msg) = -- Intentionally incomplete pattern match. Should never get here.
                        primToCall (Just p) q = Prim p (Value $ renumberMessage' globalToLocal q)
                        primToCall Nothing q = Call ANSWER [Value $ renumberMessage' globalToLocal q]
                    (extraBindings, e) <- loop False
                    addCaseFor autoCtxt f patterns e
                    debugCode
                    return (workspaceId, M.union extraBindings varEnv', e)
    return match

makeInterpreterScheduler :: Bool -> AutoSchedulerContext extra -> WorkspaceId -> IO SchedulerFn
makeInterpreterScheduler isSequential autoCtxt initWorkspaceId = do
    let !ctxt = schedulerContext autoCtxt

    requestChan <- newChan :: IO (Chan (Maybe WorkspaceId))
    initResponseMVar <- newEmptyMVar :: IO (MVar Event) -- This gets leaked but who cares.
    responseMVarsRef <- newIORef (M.singleton initWorkspaceId initResponseMVar)

    let blockOnUser _ workspaceId = do
            responseMVar <- newEmptyMVar
            modifyIORef' responseMVarsRef (M.insert workspaceId responseMVar)
            writeChan requestChan (Just workspaceId)
            takeMVar responseMVar
        replyFromUser workspaceId evt = do
            Just responseMVar <- atomicModifyIORef' responseMVarsRef (swap . M.updateLookupWithKey (\_ _ -> Nothing) workspaceId)
            putMVar responseMVar evt
            readChan requestChan
        begin = do
            Create msg <- takeMVar initResponseMVar -- TODO: Better error handling.
            pattern <- relabelMessage ctxt =<< normalize ctxt =<< generalize ctxt msg
            firstWorkspaceId <- createWorkspace ctxt False initWorkspaceId msg pattern
            return (firstWorkspaceId, LetFun ANSWER (Call ANSWER [Value msg]))

    spawnInterpreter blockOnUser begin (writeChan requestChan Nothing) isSequential autoCtxt

    let scheduler _ workspace (Send ws msg) = do
            -- TODO: Think about this and support it if it makes sense.
            -- sendMessage ctxt workspace ws msg
            putStrLn "makeInterpreterScheduler: Message sending not supported."
            Just <$> getWorkspace ctxt (identity workspace)
        scheduler _ workspace evt = do
            mWorkspaceId <- replyFromUser (identity workspace) evt
            case mWorkspaceId of
                Just workspaceId -> Just <$> getWorkspace ctxt workspaceId
                Nothing -> return Nothing

    return scheduler

spawnInterpreter :: (Bool -> WorkspaceId -> IO Event)
                 -> IO (WorkspaceId, Exp')
                 -> IO ()
                 -> Bool
                 -> AutoSchedulerContext extra
                 -> IO ThreadId
spawnInterpreter blockOnUser begin end isSequential autoCtxt = do
    let !ctxt = schedulerContext autoCtxt

    argumentsRef <- newIORef (M.empty :: M.Map WorkspaceId Message)

    let giveArgument workspaceId p = modifyIORef' argumentsRef $ M.insert workspaceId p
        retrieveArgument workspaceId = atomicModifyIORef' argumentsRef (swap . M.updateLookupWithKey (\_ _ -> Nothing) workspaceId)

    (primEnv, matchPrim) <- makePrimitives ctxt
    match <- makeMatcher blockOnUser matchPrim giveArgument retrieveArgument autoCtxt

    forkIO $ do
        (firstWorkspaceId, startExp) <- begin
        t <- {-withTaskGroup 1000 {- TODO: number of threads in pool -} $ \g ->-} do
                let execMany workspaceId args = do
                        qIds <- pendingQuestions ctxt workspaceId
                        if null qIds then do -- Then either Var case or no arguments case, either way we can reuse the current workspaceId.
                            mapM ($ workspaceId) args
                          else do -- we have precreated workspaces for the Call ANSWER or Prim case.
                            -- TODO: Need to fallback to sequential execution if we run out of threads.
                            (if isSequential then sequenceA else mapConcurrently id {- mapTasks g -}) $ zipWith ($) args qIds
                evaluateExp execMany match substitute primEnv firstWorkspaceId startExp
        t <- fullyExpand ctxt t
        T.putStrLn (toText (messageToBuilder t))
        end
