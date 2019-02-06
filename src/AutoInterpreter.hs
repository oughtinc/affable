{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module AutoInterpreter where
import Control.Concurrent ( ThreadId, forkIO ) -- base
import Control.Concurrent.Async ( mapConcurrently ) -- async
-- import Control.Concurrent.Async.Pool ( withTaskGroup, mapTasks ) -- async-pool
import Control.Concurrent.MVar ( MVar, newEmptyMVar, newMVar, putMVar, takeMVar, readMVar, modifyMVar_ ) -- base
import Control.Concurrent.Chan ( Chan, newChan, readChan, writeChan ) -- base TODO: Use TChan instead?
import qualified Control.Exception as IO ( bracket_ ) -- base
import Control.Monad ( ap, zipWithM, zipWithM_ ) -- base
import Control.Monad.IO.Class ( MonadIO, liftIO ) -- base
import Data.Foldable ( asum, forM_ ) -- base
import Data.IORef ( IORef, newIORef, readIORef, writeIORef, atomicModifyIORef', modifyIORef' ) -- base
import Data.Int ( Int64 ) -- base
import qualified Data.Map as M -- containers
import Data.Maybe ( mapMaybe ) -- base
import Data.String ( fromString ) -- base
import Data.Text ( Text ) -- text
import qualified Data.Text as T -- text
import qualified Data.Text.IO as T -- text
import qualified Data.Text.Lazy.Builder.Int as T ( decimal ) -- text
import Data.Text.Lazy.Builder ( Builder, singleton, fromText ) -- text
import Data.Traversable ( traverse ) -- base
import Data.Tuple ( swap ) -- base
import System.IO ( stderr) -- base

import AutoScheduler ( AutoSchedulerContext(..), ProcessId )
import Exp ( Result, Exp(..), Exp', Name(..), VarEnv, Var, Value, Primitive, Pattern, Kont1(..), GoFn, MatchFn, Konts(..), FunEnv, Konts',
             EvalState', varEnvToBuilder, funEnvToBuilder, nameToBuilder, nameParser, evaluateExp', applyKonts, expToBuilder, expToHaskell )
import Message ( Message(..), Pointer, PointerRemapping, messageToBuilder, matchMessage, messageParser',
                 matchPointers, expandPointers, substitute, renumberMessage' )
import Primitive ( makePrimitives )
import Scheduler ( Event(..), SchedulerContext(..), SchedulerFn, relabelMessage, fullyExpand )
import Util ( toText, invertMap )
import Workspace ( WorkspaceId, Workspace(..), emptyWorkspace )

class MonadFork m where
    fork :: ProcessId -> m () -> m ThreadId
    bracket_ :: m a -> m b -> m c -> m c
    myProcessId :: m ProcessId

instance MonadFork IO where
    fork _ = forkIO
    bracket_ = IO.bracket_
    myProcessId = return 0

newtype M a = M { runM :: ProcessId -> IO a }

instance Functor M where
    fmap f (M g) = M (fmap f . g)

instance Applicative M where
    pure = return
    (<*>) = ap

instance Monad M where
    return x = M (\_ -> return x)
    M f >>= k = M (\r -> do x <- f r; runM (k x) r)

instance MonadIO M where
    liftIO act = M (const act)

instance MonadFork M where
    fork pId (M f) = M (\_ -> forkIO (f pId))
    bracket_ (M before) (M after) (M body) = M (\r -> bracket_ (before r) (after r) (body r))
    myProcessId = M return

-- NOTE: Instead of using forkIO and co, we could use a monad other than IO for
-- expression evaluation that supports suspending a computation or implements cooperative
-- concurrency.

makeMatcher :: (MonadIO m, MonadFork m)
            => (Bool -> WorkspaceId -> m Event)
            -> (Value -> Maybe Primitive)
            -> (WorkspaceId -> Message -> m ())
            -> (WorkspaceId -> m (Maybe Message))
            -> AutoSchedulerContext extra
            -> m (GoFn m WorkspaceId Primitive Name Var -> MatchFn m WorkspaceId Primitive Name Var)
makeMatcher blockOnUser matchPrim giveArgument retrieveArgument autoCtxt = do
    let !ctxt = schedulerContext autoCtxt

    -- TODO: This could probably be made unnecessary if we started tracking timing, i.e. using that logicalTime field.
    cutOffsRef <- liftIO $ newIORef (M.empty :: M.Map WorkspaceId WorkspaceId) -- TODO: XXX Store in database for restore?

    -- Get answers that have been answered since last time the workspace was displayed.
    let getRecentlyAnswered workspace = do
            let !workspaceId = identity workspace
                !answeredMax = case mapMaybe (\(qId, _, ma) -> qId <$ ma) $ subQuestions workspace of [] -> -1; qIds -> maximum qIds
            currCutOff <- liftIO $ maybe (-1) id <$> atomicModifyIORef' cutOffsRef (swap . M.insertLookupWithKey (\_ new _ -> new) workspaceId answeredMax)
            -- Answers of workspaces that have answers and are newer than the cutoff.
            return $! mapMaybe (\(qId, _, ma) -> if qId > currCutOff then ma else Nothing) $ subQuestions workspace

    -- Mapping of pointers from replay workspace to original workspace.
    workspaceVariablesRef <- liftIO $ newIORef (M.empty :: M.Map WorkspaceId PointerRemapping) -- TODO: XXX Store in database(?) Maybe not?

    let linkVars workspaceId mapping = liftIO $ modifyIORef' workspaceVariablesRef $ M.insertWith M.union workspaceId mapping
        links workspaceId = liftIO $ (maybe M.empty id . M.lookup workspaceId) <$> readIORef workspaceVariablesRef

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
    pendingMatchesRef <- liftIO $ newIORef (M.empty :: M.Map Name (MVar [([Pattern], MVar ())]))

    -- When we receive the Submit event, we look at the unanswered questions of the current workspace for the parameters.
    -- Expand doesn't get batched, so we only have batches of questions, i.e. multi-argument function calls are always
    -- of the form `f(answer(...), prim1(...), ...)`.
    let match go s varEnv funEnv f [Reference p] k = do
            error "Did you get here? If so, tell me how."
            m <- liftIO $ dereference ctxt p
            match go s varEnv funEnv f [m] k
        match go workspaceId varEnv funEnv f ms k = do
            ms' <- liftIO $ mapM (\m -> normalize ctxt =<< generalize ctxt m) ms

            -- Atomically:
            --  - Match against each pending pattern.
            --  - If there is a match then return the corresponding lock. DON'T block on the lock here. Only after we finish processing the map.
            --  - If there isn't a match, create and insert a lock for a pattern corresponding to the current message.
            -- If we found an existing lock, then block on it and execute retryLoop when it releases.
            -- If we made a new lock, then continue with matchFailed and release the lock after we addCaseFor or if we're killed.
            --  - We also need to clean out the mapping in this case before we release the lock.
            --  - We should be able to use bracket_ to do this with a trivial acquire function as the lock should be created already locked.
            let matchPending workspace = do
                    pendingMatchesMVar <- liftIO $ do
                        fMVar <- newMVar [] -- This will get garbage collected if we don't use it, which we won't except for the first time.
                        mMVar <- atomicModifyIORef' pendingMatchesRef $ swap . M.insertLookupWithKey (\_ _ old -> old) f fMVar
                        return $ maybe fMVar id mMVar

                    -- Begin atomic block
                    pendingMatches <- liftIO $ takeMVar pendingMatchesMVar -- BLOCK
                    -- TODO: Error out if there is more than one match.
                    -- TODO: If an error happens here, pendingMatchesMVar will not be refilled and will lead to deadlock.
                    case asum $ map (\(ps, pMVar) -> pMVar <$ zipWithM matchMessage ps ms') pendingMatches of
                        Just pMVar -> do
                            liftIO $ putMVar pendingMatchesMVar pendingMatches
                            -- End atomic block
                            liftIO $ readMVar pMVar -- BLOCK until filled
                            retryLoop
                        Nothing -> do
                            pMVar <- liftIO $ newEmptyMVar -- locked lock
                            liftIO $ putMVar pendingMatchesMVar ((ms', pMVar):pendingMatches)
                            -- End atomic block
                            bracket_ (return ())
                                     (liftIO $ do modifyMVar_ pendingMatchesMVar (return . filter ((pMVar/=) . snd)); putMVar pMVar ())
                                     (matchFailed workspace ms')

                retryLoop = do
                    workspace <- liftIO $ getWorkspace ctxt workspaceId
                    alts <- liftIO $ alternativesFor autoCtxt f
                    case alts of -- TODO: Could mark workspaces as "human-influenced" when a pattern match failure is hit
                                 -- or when any subquestions are marked. This would allow "garbage collecting" workspaces
                                 -- with answers that are not "human-influenced", i.e. were created entirely through automation.
                        _:_ -> do
                            -- TODO: Error out if there is more than one match.
                            let !mMatch = asum $ map (\(ps, e) -> fmap (\bindings -> (ps, M.union (M.unions bindings) varEnv, e)) $ zipWithM matchMessage ps ms') alts
                            case mMatch of
                                Just (patterns, varEnv', e) -> do
                                    varEnv' <- liftIO $ traverse (\case Reference p -> dereference ctxt p; x -> return x) varEnv'

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
                                            arg <- liftIO $ dereference ctxt p
                                            liftIO $ expandPointer ctxt workspaceId p
                                            giveArgument workspaceId arg
                                            go (M.insert ptr arg varEnv') funEnv workspaceId e k -- TODO: Think about this.
                                        LetFun _ (Call _ args) -> do -- ask cases
                                            let !localToGlobal = invertMap globalToLocal
                                            forM_ args $ \argExp -> do
                                                let !m = case argExp of Call ANSWER [Value m] -> m; Prim _ (Value m) -> m
                                                let !arg = substitute bindings m
                                                pattern <- liftIO $ relabelMessage ctxt =<< normalize ctxt =<< generalize ctxt arg
                                                liftIO $ createWorkspace ctxt False workspaceId (renumberMessage' localToGlobal m) pattern
                                            go varEnv' funEnv workspaceId e k -- TODO: Think about this.
                                        Value msg -> do -- reply case
                                            let !msg' = substitute bindings msg
                                            msg <- liftIO $ normalize ctxt =<< case msg' of Reference p -> dereference ctxt p; _ -> relabelMessage ctxt msg'
                                            liftIO $ sendAnswer ctxt False workspaceId msg -- TODO: Can I clean out workspaceVariablesRef a bit now?
                                            go varEnv' funEnv workspaceId e k -- TODO: Think about this.
                                        -- _ -> go varEnv' funEnv workspaceId e k -- Intentionally missing this case.
                                Nothing -> matchPending workspace
                        [] -> matchPending workspace
            retryLoop
          where matchFailed workspace ms' = do
                    let !workspaceId = identity workspace
                    patterns <- liftIO $ mapM (relabelMessage ctxt) ms'
                    let !(Just bindings) = M.unions <$> zipWithM matchMessage patterns ms' -- This shouldn't fail.
                    bindings <- liftIO $ traverse (\case Reference p -> dereference ctxt p; x -> return x) bindings
                    let !varEnv' = M.union bindings varEnv

                    linkPointers f workspace patterns
                    globalToLocal <- links workspaceId

                    let loop stay = blockOnUser stay workspaceId >>= processEvent
                        processEvent (Create msg) = do
                            pattern <- liftIO $ relabelMessage ctxt =<< normalize ctxt =<< generalize ctxt msg
                            liftIO $ createWorkspace ctxt False workspaceId msg pattern
                            loop True
                        processEvent (Expand ptr) = do -- TODO: Make this more resilient to pointers that are not in scope.
                            liftIO $ expandPointer ctxt workspaceId ptr
                            arg <- liftIO $ dereference ctxt ptr
                            giveArgument workspaceId arg
                            g <- liftIO $ newFunction autoCtxt
                            let !ptr' = maybe ptr id $ M.lookup ptr globalToLocal
                            return (M.singleton ptr' arg, LetFun g (Call g [Var ptr']))
                        processEvent (Answer msg@(Structured [Reference p])) = do -- dereference pointers -- TODO: Do this?
                            msg' <- liftIO $ dereference ctxt p
                            liftIO $ sendAnswer ctxt False workspaceId msg' -- TODO: Can I clean out workspaceVariablesRef a bit now?
                            return (M.empty, Value $ renumberMessage' globalToLocal msg)
                        processEvent (Answer msg) = do
                            msg' <- liftIO $ relabelMessage ctxt =<< normalize ctxt msg
                            liftIO $ sendAnswer ctxt False workspaceId msg' -- TODO: Can I clean out workspaceVariablesRef a bit now?
                            return (M.empty, Value $ renumberMessage' globalToLocal msg)
                        processEvent Submit = do
                            workspace <- liftIO $ getWorkspace ctxt workspaceId -- Refresh workspace.
                            -- Get unanswered questions.
                            let !qs = mapMaybe (\(_, q, ma) -> maybe (Just q) (\_ -> Nothing) ma) $ subQuestions workspace
                            if null qs then loop False else do -- If qs is empty there's nothing to wait on so just do nothing.
                                g <- liftIO $ newFunction autoCtxt
                                let args = map (\q -> primToCall (matchPrim q) q) qs
                                return (M.empty, LetFun g (Call g args))
                        -- processEvent (Send ws msg) = -- Intentionally incomplete pattern match. Should never get here.
                        primToCall (Just p) q = Prim p (Value $ renumberMessage' globalToLocal q)
                        primToCall Nothing q = Call ANSWER [Value $ renumberMessage' globalToLocal q]
                    (extraBindings, e) <- loop False
                    liftIO $ addCaseFor autoCtxt f patterns e
                    liftIO $ debugCode
                    go (M.union extraBindings varEnv') funEnv workspaceId e k
    return match

makeInterpreterScheduler :: (MonadIO m, MonadFork m) => Bool -> AutoSchedulerContext extra -> WorkspaceId -> m SchedulerFn
makeInterpreterScheduler isSequential autoCtxt initWorkspaceId = do
    let !ctxt = schedulerContext autoCtxt

    requestChan <- liftIO (newChan :: IO (Chan (Maybe WorkspaceId)))
    initResponseMVar <- liftIO (newEmptyMVar :: IO (MVar Event)) -- This gets leaked but who cares.
    responseMVarsRef <- liftIO $ newIORef (M.singleton initWorkspaceId initResponseMVar)

    let blockOnUser _ workspaceId = do
            responseMVar <- liftIO $ newEmptyMVar
            liftIO $ modifyIORef' responseMVarsRef (M.insert workspaceId responseMVar)
            liftIO $ writeChan requestChan (Just workspaceId)
            liftIO $ takeMVar responseMVar -- BLOCK
        replyFromUser workspaceId evt = do
            Just responseMVar <- liftIO $ atomicModifyIORef' responseMVarsRef (swap . M.updateLookupWithKey (\_ _ -> Nothing) workspaceId)
            liftIO $ putMVar responseMVar evt
            liftIO $ readChan requestChan
        begin = do
            Create msg <- liftIO $ takeMVar initResponseMVar -- TODO: Better error handling.
            pattern <- liftIO $ relabelMessage ctxt =<< normalize ctxt =<< generalize ctxt msg
            firstWorkspaceId <- liftIO $ createWorkspace ctxt False initWorkspaceId msg pattern
            return (firstWorkspaceId, LetFun ANSWER (Call ANSWER [Value msg]))

    spawnInterpreter blockOnUser begin (liftIO $ writeChan requestChan Nothing) isSequential autoCtxt

    let scheduler _ workspace (Send ws msg) = do
            -- TODO: Think about this and support it if it makes sense.
            -- sendMessage ctxt workspace ws msg
            liftIO $ putStrLn "makeInterpreterScheduler: Message sending not supported."
            liftIO $ Just <$> getWorkspace ctxt (identity workspace)
        scheduler _ workspace evt = do
            mWorkspaceId <- replyFromUser (identity workspace) evt
            case mWorkspaceId of
                Just workspaceId -> liftIO $ Just <$> getWorkspace ctxt workspaceId
                Nothing -> return Nothing

    return scheduler

concurrentlyK :: (MonadIO m, MonadFork m)
              => GoFn m WorkspaceId Primitive Name Var
              -> MatchFn m WorkspaceId Primitive Name Var
              -> AutoSchedulerContext extra
              -> VarEnv Var
              -> FunEnv Name Var
              -> [WorkspaceId]
              -> [Exp']
              -> Konts'
              -> m Result
concurrentlyK go match autoCtxt varEnv funEnv ss es k@(CallKont _ f workspaceId _) = do
    let kId = (workspaceId, f)
        !n = length ss -- should equal length es, TODO: Add assertion.
    forM_ (zip3 [0..] ss es) $ \(i, s, e) -> do
        pId <- liftIO (newProcess autoCtxt)
        fork pId (() <$ go varEnv funEnv s e (NotifyKont i n kId))
    return Nothing

spawnInterpreter :: (MonadIO m, MonadFork m)
                 => (Bool -> WorkspaceId -> m Event)
                 -> m (WorkspaceId, Exp')
                 -> m ()
                 -> Bool
                 -> AutoSchedulerContext extra
                 -> m ThreadId
spawnInterpreter blockOnUser begin end isSequential autoCtxt = do
    let !ctxt = schedulerContext autoCtxt

    argumentsRef <- liftIO $ newIORef (M.empty :: M.Map WorkspaceId Message)

    let giveArgument workspaceId p = liftIO $ modifyIORef' argumentsRef $ M.insert workspaceId p
        retrieveArgument workspaceId = liftIO $ atomicModifyIORef' argumentsRef (swap . M.updateLookupWithKey (\_ _ -> Nothing) workspaceId)

    (primEnv', matchPrim) <- liftIO $ makePrimitives ctxt
    let !primEnv = fmap (\f x y -> liftIO (f x y)) primEnv'
    match' <- makeMatcher blockOnUser matchPrim giveArgument retrieveArgument autoCtxt

    pId <- liftIO (newProcess autoCtxt)
    fork pId $ do
        (firstWorkspaceId, startExp) <- begin
        mt <- do
                let execMany go varEnv funEnv workspaceId es k@(CallKont _ f s _) = do
                        let kId = (s, f)
                        liftIO $ saveContinuation autoCtxt k -- TODO: Think about this.
                        qIds <- liftIO $ pendingQuestions ctxt workspaceId
                        if null qIds then do -- Then either Var case or no arguments case, either way we can reuse the current workspaceId.
                            case es of
                                [] -> applyKonts match k []
                                [e] -> concurrentlyK go match autoCtxt varEnv funEnv [workspaceId] es k
                                -- Intentionally omitting length es > 1 case which shouldn't happen.
                          else do -- we have precreated workspaces for the Call ANSWER or Prim case.
                                concurrentlyK go match autoCtxt varEnv funEnv qIds es k

                    match = match' go
                    notifyKont kId 0 1 v = do
                        liftIO $ addContinuationArgument autoCtxt kId 0 v
                        k <- liftIO $ loadContinuation autoCtxt kId
                        applyKonts match k [v]
                    notifyKont kId argNumber numArgs v = do
                        -- Check to see if all the arguments are available for the continuation
                        -- that kId refers to, and if so, continue with it, else terminate.
                        liftIO $ addContinuationArgument autoCtxt kId argNumber v
                        (k, vs) <- liftIO $ continuationArguments autoCtxt kId -- TODO: This could be more efficient.
                        if length vs == numArgs then do
                            applyKonts match k vs
                          else do
                            return Nothing
                    go varEnv funEnv s e k = do
                        v <- evaluateExp' (\s -> do pId <- myProcessId; liftIO (recordState autoCtxt pId s)) (execMany go) match notifyKont substitute primEnv varEnv funEnv s e k
                        liftIO . terminate autoCtxt =<< myProcessId
                        return v

                go M.empty M.empty firstWorkspaceId startExp Done
        case mt of
            Nothing -> undefined-- TODO: XXX Here we would trampoline on t. To that end, we would eliminate the uses of fork.
            Just t -> do
                t <- liftIO $ fullyExpand ctxt t
                liftIO $ T.putStrLn (toText (messageToBuilder t))
                end
