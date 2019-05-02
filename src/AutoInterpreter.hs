{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module AutoInterpreter where
import Control.Concurrent ( ThreadId, forkIO ) -- base
import Control.Concurrent.Async ( mapConcurrently ) -- async
-- import Control.Concurrent.Async.Pool ( withTaskGroup, mapTasks ) -- async-pool
import Control.Concurrent.STM ( atomically ) -- stm
import Control.Concurrent.STM.TMVar ( TMVar, newEmptyTMVarIO, newEmptyTMVar, newTMVarIO, putTMVar, takeTMVar, readTMVar ) -- stm
import Control.Concurrent.STM.TChan ( TChan, newTChanIO, readTChan, writeTChan ) -- stm
import qualified Control.Exception as IO ( bracket_ ) -- base
import Control.Monad ( ap, foldM, zipWithM, zipWithM_ ) -- base
import Control.Monad.IO.Class ( MonadIO, liftIO ) -- base
import Data.Foldable ( asum, forM_ ) -- base
import Data.IORef ( IORef, newIORef, readIORef, writeIORef, atomicModifyIORef', modifyIORef' ) -- base
import Data.Int ( Int64 ) -- base
import qualified Data.Map as M -- containers
import Data.Maybe ( mapMaybe, catMaybes ) -- base
import Data.String ( fromString ) -- base
import Data.Text ( Text ) -- text
import qualified Data.Text as T -- text
import qualified Data.Text.IO as T -- text
import qualified Data.Text.Lazy.Builder.Int as T ( decimal ) -- text
import Data.Text.Lazy.Builder ( Builder, singleton, fromText ) -- text
import Data.Traversable ( traverse, forM ) -- base
import Data.Tuple ( swap ) -- base
import qualified Data.UUID as UUID -- uuid
import System.IO ( stderr) -- base

import AutoScheduler ( AutoSchedulerContext(..), ProcessId, AddContinuationResult(..), newFunction )
import Exp ( Result(..), Exp(..), Exp', Name(..), VarEnv, Var, Value, Primitive, Pattern, Kont1(..), GoFn, MatchFn, Konts(..), FunEnv, Konts',
             EvalState', varEnvToBuilder, funEnvToBuilder, nameToBuilder, nameParser, evaluateExp', applyKonts, expToBuilder, expToHaskell )
import Message ( Message(..), Pointer, PointerRemapping, messageToBuilder, matchMessage, messageParser',
                 stripLabel, matchPointers, expandPointers, substitute, renumberMessage' )
import Primitive ( makePrimitives )
import Scheduler ( UserId, Event(..), SchedulerContext(..), SchedulerFn,
                   autoUserId, relabelMessage, fullyExpand, normalize, generalize, canonicalizeEvents )
import Util ( toText, invertMap )
import Workspace ( VersionId, Workspace(..) )

class MonadFork m where
    fork :: m () -> m ThreadId
    bracket_ :: m a -> m b -> m c -> m c
    myProcessId :: m ProcessId
    withProcessId :: ProcessId -> m a -> m a
    mapTasks :: [m a] -> m [a]

instance MonadFork IO where
    fork = forkIO
    bracket_ = IO.bracket_
    myProcessId = return UUID.nil
    withProcessId _ = id
    mapTasks = mapConcurrently id

newtype M a = M { unM :: ProcessId -> IO a }

runM :: M a -> IO a
runM (M act) = act UUID.nil

instance Functor M where
    fmap f (M g) = M (fmap f . g)

instance Applicative M where
    pure = return
    (<*>) = ap

instance Monad M where
    return x = M (\_ -> return x)
    M f >>= k = M (\r -> do x <- f r; unM (k x) r)

instance MonadIO M where
    liftIO act = M (const act)

instance MonadFork M where
    fork (M f) = M (forkIO . f)
    bracket_ (M before) (M after) (M body) = M (\r -> bracket_ (before r) (after r) (body r))
    myProcessId = M return
    withProcessId pId (M f) = M (const (f pId))
    mapTasks ts = M (\pId -> mapConcurrently (\t -> unM t pId) ts)

-- NOTE: Instead of using forkIO and co, we could use a monad other than IO for
-- expression evaluation that supports suspending a computation or implements cooperative
-- concurrency.

makeMatcher :: (MonadIO m, MonadFork m)
            => (VersionId -> m (UserId, Event))
            -> (Value -> Maybe Primitive)
            -> (VersionId -> Message -> m ())
            -> (VersionId -> m (Maybe Message))
            -> AutoSchedulerContext extra
            -> m (GoFn m VersionId Primitive Name Var ProcessId -> MatchFn m VersionId Primitive Name Var ProcessId)
makeMatcher blockOnUser matchPrim giveArgument retrieveArgument autoCtxt = do
    let !ctxt = schedulerContext autoCtxt

        -- If retrieveArgument produces something then we've recently done a variable lookup and need
        -- to map pointers from it. Otherwise we just map pointers from all the answers that have been
        -- added in the latest batch.
        linkPointers ANSWER workspace vId [pattern] = do
            liftIO $ linkVars autoCtxt vId $ matchPointers pattern (question workspace)
        linkPointers _ workspace vId patterns = do
            mDeref <- retrieveArgument (identity workspace)
            case (mDeref, patterns) of
                (Just a, [pattern]) -> liftIO $ linkVars autoCtxt vId $ matchPointers pattern a
                (Nothing, _) -> do
                    workspace <- liftIO $ getWorkspace ctxt vId
                    let !answered = mapMaybe (\(_, _, ma) -> ma) (subQuestions workspace)
                    liftIO $ zipWithM_ (\pattern a -> linkVars autoCtxt vId $ matchPointers pattern a)
                                       (reverse patterns)
                                       (reverse answered)
                -- Intentionally incomplete.

        debugCode = do
            altMap <- allAlternatives autoCtxt
            -- T.hPutStrLn stderr (toText (expToHaskell (\f -> maybe [] reverse $ M.lookup f altMap) (LetFun ANSWER (Value (Text "dummy")))))
            T.hPutStrLn stderr (toText (expToBuilder (\f -> maybe [] reverse $ M.lookup f altMap) (LetFun ANSWER (Value (Text "dummy")))))

    -- Store matches that are being worked on. Definitely does NOT need to be in the database.
    pendingMatchesRef <- liftIO $ newIORef (M.empty :: M.Map Name (TMVar [([Pattern], TMVar ())]))

    -- When we receive the Submit event, we look at the unanswered questions of the current workspace for the parameters.
    -- Expand doesn't get batched, so we only have batches of questions, i.e. multi-argument function calls are always
    -- of the form `f(answer(...), prim1(...), ...)`.
    let match eval s varEnv funEnv f [Reference p] k = do
            error "Did you get here? If so, tell me how."
            m <- liftIO $ dereference ctxt p
            match eval s varEnv funEnv f [m] k
        match eval versionId varEnv funEnv f ms k = do
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
                    pendingMatchesTMVar <- liftIO $ do
                        fTMVar <- newTMVarIO [] -- This will get garbage collected if we don't use it, which we won't except for the first time.
                        mTMVar <- atomicModifyIORef' pendingMatchesRef $ swap . M.insertLookupWithKey (\_ _ old -> old) f fTMVar
                        return $ maybe fTMVar id mTMVar

                    r <- liftIO $ atomically $ do
                            pendingMatches <- takeTMVar pendingMatchesTMVar -- BLOCK
                            -- TODO: Error out if there is more than one match.
                            case asum $ map (\(ps, pTMVar) -> pTMVar <$ zipWithM matchMessage ps ms') pendingMatches of
                                r@(Just pTMVar) -> do
                                    putTMVar pendingMatchesTMVar pendingMatches
                                    return (Right pTMVar)
                                r@Nothing -> do
                                    pTMVar <- newEmptyTMVar -- locked lock
                                    putTMVar pendingMatchesTMVar ((ms', pTMVar):pendingMatches)
                                    return (Left pTMVar)
                    case r of
                        Right pTMVar -> do
                            liftIO $ atomically $ readTMVar pTMVar -- BLOCK until filled
                            retryLoop
                        Left pTMVar -> do
                            bracket_ (return ())
                                     (liftIO $ do
                                        atomically $ do
                                            pendingMatches <- takeTMVar pendingMatchesTMVar
                                            putTMVar pendingMatchesTMVar (filter ((pTMVar /=) . snd) pendingMatches)
                                        atomically $ putTMVar pTMVar ())
                                     (matchFailed workspace ms')

                retryLoop = do
                    workspace <- liftIO $ getWorkspace ctxt versionId
                    alts <- liftIO $ alternativesFor autoCtxt f
                    case alts of -- TODO: Could mark workspaces as "human-influenced" when a pattern match failure is hit
                                 -- or when any subquestions are marked. This would allow "garbage collecting" workspaces
                                 -- with answers that are not "human-influenced", i.e. were created entirely through automation.
                        _:_ -> do
                            -- TODO: Error out if there is more than one match.
                            let !mMatch = asum $ map (\(ps, e) -> fmap (\bindings -> (ps, M.union (M.unions bindings) varEnv, e)) $ zipWithM matchMessage ps ms') alts
                            case mMatch of
                                Just (patterns, varEnv', e) -> do
                                    (vId, _) <- liftIO $ newVersion ctxt versionId -- TODO: I want to have a way to not use a new version.
                                    varEnv' <- liftIO $ traverse (\case Reference p -> dereference ctxt p; x -> return x) varEnv'

                                    -- This is to make it so occurrences of variables bound by as-patterns don't get substituted.
                                    let bindings = M.union (M.unions $
                                                                zipWith (\p m -> case (p, m) of
                                                                                    (LabeledStructured asP _, LabeledStructured l _) -> M.singleton asP (Reference l)
                                                                                    _ -> M.empty)
                                                                      patterns ms') varEnv'

                                    linkPointers f workspace vId patterns
                                    globalToLocal <- liftIO $ links autoCtxt vId

                                    -- This is a bit hacky. If this approach is the way to go, make these patterns individual constructors.
                                    case e of
                                        LetFun _ (Call _ [Var ptr]) -> do -- expand case
                                            let !p = maybe ptr id $ M.lookup ptr $ invertMap globalToLocal
                                            arg <- liftIO $ dereference ctxt p
                                            liftIO $ expandPointer ctxt autoUserId vId p
                                            giveArgument vId arg -- TODO: Or versionId?
                                            eval (M.insert ptr arg varEnv') funEnv vId e k -- TODO: Think about this.
                                        LetFun _ (Call _ args) -> do -- ask cases
                                            let !localToGlobal = invertMap globalToLocal
                                            forM_ args $ \argExp -> do
                                                    let !m = case argExp of Call ANSWER [Value m] -> m; Prim _ (Value m) -> m
                                                    let !arg = substitute bindings m
                                                    pattern <- liftIO $ relabelMessage ctxt =<< normalize ctxt =<< generalize ctxt arg
                                                    () <$ liftIO (createWorkspace ctxt autoUserId vId
                                                                    (renumberMessage' localToGlobal m) pattern)
                                            eval varEnv' funEnv vId e k -- TODO: Think about this.
                                        Value msg -> do -- reply case
                                            let !msg' = substitute bindings msg
                                            msg <- liftIO $ normalize ctxt =<< case msg' of Reference p -> dereference ctxt p; _ -> relabelMessage ctxt msg'
                                            liftIO $ sendAnswer ctxt autoUserId vId msg
                                            eval varEnv' funEnv vId e k -- TODO: Think about this.
                                        -- _ -> eval varEnv' funEnv vId e k -- Intentionally missing this case.
                                Nothing -> matchPending workspace
                        [] -> matchPending workspace
            retryLoop
          where matchFailed workspace ms' = do
                    let !versionId = identity workspace
                    (vId, _) <- liftIO $ newVersion ctxt versionId -- TODO: I want to have a way to not use a new version.
                    patterns <- liftIO $ mapM (relabelMessage ctxt) ms'
                    let !(Just bindings) = M.unions <$> zipWithM matchMessage patterns ms' -- This shouldn't fail.
                    bindings <- liftIO $ traverse (\case Reference p -> dereference ctxt p; x -> return x) bindings
                    let !varEnv' = M.union bindings varEnv

                    linkPointers f workspace vId patterns
                    globalToLocal <- liftIO $ links autoCtxt vId

                    let loop = blockOnUser vId >>= processEvent
                        processEvent (userId, Create msg) = do
                            pattern <- liftIO $ relabelMessage ctxt =<< normalize ctxt =<< generalize ctxt msg
                            (_, _, _) <- liftIO $ createWorkspace ctxt userId vId msg pattern
                            loop
                        processEvent (userId, Expand ptr) = do -- TODO: Make this more resilient to pointers that are not in scope.
                            liftIO $ expandPointer ctxt userId vId ptr
                            arg <- liftIO $ dereference ctxt ptr
                            giveArgument vId arg -- TODO: Or versionId?
                            g <- liftIO $ newFunction autoCtxt
                            let !ptr' = maybe ptr id $ M.lookup ptr globalToLocal
                            return (M.singleton ptr' arg, LetFun g (Call g [Var ptr']))
                        processEvent (userId, Answer msg@(Structured [Reference p])) = do -- dereference pointers -- TODO: Do this?
                            msg' <- liftIO $ dereference ctxt p
                            liftIO $ sendAnswer ctxt userId vId msg'
                            return (M.empty, Value $ renumberMessage' globalToLocal msg)
                        processEvent (userId, Answer msg) = do
                            msg' <- liftIO $ relabelMessage ctxt =<< normalize ctxt msg
                            liftIO $ sendAnswer ctxt userId vId msg'
                            return (M.empty, Value $ renumberMessage' globalToLocal msg)
                        -- processEvent (userId, Send ws msg) = -- Intentionally incomplete pattern match. Should never get here.
                        processEvent (userId, Submit) = do
                            workspace <- liftIO $ getWorkspace ctxt vId -- Refresh workspace.
                            -- Get unanswered questions.
                            let !qs = mapMaybe (\(_, q, ma) -> maybe (Just q) (\_ -> Nothing) ma) $ subQuestions workspace
                            if null qs then loop else do -- If qs is empty there's nothing to wait on so just do nothing.
                                g <- liftIO $ newFunction autoCtxt
                                let args = map (\q -> primToCall (matchPrim q) q) qs
                                return (M.empty, LetFun g (Call g args))
                        processEvent (userId, Init) = error "processEvent Init: Shouldn't happen."

                        primToCall (Just p) q = Prim p (Value $ renumberMessage' globalToLocal q)
                        primToCall Nothing q = Call ANSWER [Value $ renumberMessage' globalToLocal q]

                    (extraBindings, e) <- loop
                    liftIO $ addCaseFor autoCtxt f patterns e
                    liftIO $ debugCode
                    eval (M.union extraBindings varEnv') funEnv vId e k
    return match

makeInterpreterScheduler :: (MonadIO m, MonadFork m) => Bool -> AutoSchedulerContext extra -> VersionId -> m SchedulerFn
makeInterpreterScheduler isSequential autoCtxt initWorkspaceId = do
    let !ctxt = schedulerContext autoCtxt

    requestTChan <- liftIO (newTChanIO :: IO (TChan (Maybe VersionId)))
    responseTMVarsRef <- liftIO $ newIORef (M.empty :: M.Map VersionId (TMVar (UserId, Event))) -- TODO: Make this use workspaceIds?

    let blockOnUser versionId = liftIO $ do
            responseTMVar <- newEmptyTMVarIO
            modifyIORef' responseTMVarsRef (M.insert versionId responseTMVar)
            atomically $ writeTChan requestTChan (Just versionId)
            atomically $ takeTMVar responseTMVar -- BLOCK

        replyFromUser userId versionId Init = liftIO $ do
            atomically $ readTChan requestTChan
        replyFromUser userId versionId evt = liftIO $ do
            [evt] <- canonicalizeEvents ctxt [evt]
            Just responseTMVar <- atomicModifyIORef' responseTMVarsRef (swap . M.updateLookupWithKey (\_ _ -> Nothing) versionId)
            atomically $ putTMVar responseTMVar (userId, evt)
            atomically $ readTChan requestTChan
        begin = liftIO $ do
            initWorkspace <- getWorkspace ctxt initWorkspaceId
            let !q = stripLabel (question initWorkspace)
            return (initWorkspaceId, LetFun ANSWER (Call ANSWER [Value q]))

    spawnInterpreter blockOnUser begin (liftIO $ atomically $ writeTChan requestTChan Nothing) isSequential autoCtxt

    let scheduler _ workspace (Send ws msg) = liftIO $ do
            -- TODO: Think about this and support it if it makes sense.
            -- sendMessage ctxt workspace ws msg
            putStrLn "makeInterpreterScheduler: Message sending not supported."
            Just <$> getWorkspace ctxt (identity workspace)
        scheduler userId workspace evt = do
            mWorkspaceId <- replyFromUser userId (identity workspace) evt
            case mWorkspaceId of
                Just versionId -> liftIO $ Just <$> getWorkspace ctxt versionId
                Nothing -> return Nothing

    return scheduler

concurrentlyK :: (MonadIO m, MonadFork m)
              => MatchFn m VersionId Primitive Name Var ProcessId
              -> AutoSchedulerContext extra
              -> VarEnv Var
              -> FunEnv Name Var
              -> [VersionId]
              -> [Exp']
              -> Konts'
              -> m (Result ProcessId)
concurrentlyK match autoCtxt varEnv funEnv ss es k@(CallKont _ f versionId _) = do
    let kId = (versionId, f)
        !n = length ss -- should equal length es, TODO: Add assertion.
    processId <- myProcessId
    pIds <- liftIO $ forM (zip3 [0..] ss es) $ \(i, s, e) -> do
        pId <- newProcess autoCtxt
        recordState autoCtxt pId (varEnv, funEnv, s, e, NotifyKont i n kId)
        return pId
    liftIO $ terminate autoCtxt processId-- TODO: Alternatively, have this process handle one of the e's.
    return $ Died pIds

spawnInterpreter :: (MonadIO m, MonadFork m)
                 => (VersionId -> m (UserId, Event))
                 -> m (VersionId, Exp')
                 -> m ()
                 -> Bool
                 -> AutoSchedulerContext extra
                 -> m ThreadId
spawnInterpreter blockOnUser begin end isSequential autoCtxt = do
    let !ctxt = schedulerContext autoCtxt

    argumentsRef <- liftIO $ newIORef (M.empty :: M.Map VersionId Message)

    let giveArgument versionId p = liftIO $ do
            modifyIORef' argumentsRef $ M.insert versionId p
        retrieveArgument versionId = liftIO $ do
            atomicModifyIORef' argumentsRef (swap . M.updateLookupWithKey (\_ _ -> Nothing) versionId)

    (primEnv', matchPrim) <- liftIO $ makePrimitives ctxt
    let !primEnv = fmap (\f x y -> liftIO (f x y)) primEnv'
    match' <- makeMatcher blockOnUser matchPrim giveArgument retrieveArgument autoCtxt

    fork $ do
        q <- liftIO $ runQueue autoCtxt
        q <- if null q then do
                (firstWorkspaceId, startExp) <- begin
                initProcessId <- liftIO (newProcess autoCtxt)
                liftIO $ recordState autoCtxt initProcessId (M.empty, M.empty, firstWorkspaceId, startExp, Done)
                return [initProcessId]
              else do
                return q -- TODO: Think through what needs to be done for initialization, if anything.

        let execMany varEnv funEnv versionId es k@(CallKont _ f s _) = do
                let kId = (s, f)
                liftIO $ saveContinuation autoCtxt k
                qIds <- liftIO $ pendingQuestions ctxt versionId
                if null qIds then do -- Then either Var case or no arguments case, either way we can reuse the current versionId.
                    case es of
                        [] -> applyKonts match k []
                        [e] -> concurrentlyK match autoCtxt varEnv funEnv [versionId] es k
                        -- Intentionally omitting length es > 1 case which shouldn't happen.
                  else do -- we have precreated workspaces for the Call ANSWER or Prim case.
                    concurrentlyK match autoCtxt varEnv funEnv qIds es k

            record s = do
                pId <- myProcessId
                liftIO $ recordState autoCtxt pId s

            match = match' (\varEnv funEnv s e k -> do record (varEnv, funEnv, s, e, k); return Paused)

            die = do
                liftIO . terminate autoCtxt =<< myProcessId
                return $ Died []

            notifyKont kId 0 1 v = do
                r <- liftIO $ addContinuationArgument autoCtxt kId 0 v
                case r of
                    SAME -> die
                    -- REPLACED -> undefined -- TODO XXX
                    _ -> do
                        k <- liftIO $ loadContinuation autoCtxt kId
                        applyKonts match k [v]
            notifyKont kId argNumber numArgs v = do
                -- Check to see if all the arguments are available for the continuation
                -- that kId refers to, and if so, continue with it, else terminate.
                r <- liftIO $ addContinuationArgument autoCtxt kId argNumber v
                case r of
                    SAME -> die
                    -- REPLACED -> undefined -- TODO XXX
                    _ -> do
                        (k, vs) <- liftIO $ continuationArguments autoCtxt kId -- TODO: This could be more efficient.
                        if length vs == numArgs then applyKonts match k vs else die

            eval = evaluateExp' record execMany match notifyKont substitute primEnv

            -- TODO: These effectively do the scheduling currently. We could make these functions parameters
            -- to allow a more thoughtful choice.
            consumeConcurrent [] = do
                q <- liftIO $ runQueue autoCtxt
                if null q then return (error "AutoInterpreter.hs:consumeConcurrent: Should this happen?") -- TODO
                          else consumeConcurrent q
            consumeConcurrent pIds = do
                let go pIds = do
                        asum <$> mapTasks (map (\pId -> do
                            let loop = do
                                    (varEnv, funEnv, s, e, k) <- liftIO $ currentState autoCtxt pId
                                    r <- withProcessId pId (eval varEnv funEnv s e k)
                                    case r of
                                        Finished v -> do
                                            liftIO $ terminate autoCtxt pId
                                            return $ Just v
                                        Paused -> loop
                                        Died [] -> return Nothing
                                        Died pIds -> go pIds
                            loop) pIds)
                Just v <- go pIds
                return v

            -- NOTE: I think the way this is implemented will produce something like a breadth-first traversal.
            -- A more depth-first traversal should be able to be produced by reading the RunQueue every step and
            -- always executing the most recently added process. To that end, we'd probably want to add tasks in
            -- reverse order or it will be depth-first, right-to-left order.
            consumeSequential [] = do
                q <- liftIO $ runQueue autoCtxt
                if null q then return (error "AutoInterpreter.hs:consumeSequential: Should this happen?") -- TODO
                          else consumeSequential q
            consumeSequential (pId:pIds) = do
                state@(varEnv, funEnv, s, e, k) <- liftIO $ currentState autoCtxt pId
                mv <- withProcessId pId $ eval varEnv funEnv s e k
                case mv of
                    Finished v -> do
                        liftIO $ terminate autoCtxt pId
                        return v
                    _ -> consumeSequential pIds

        t <- (if isSequential then consumeSequential else consumeConcurrent) q

        t <- liftIO $ fullyExpand ctxt t
        liftIO $ T.putStrLn (toText (messageToBuilder t))
        end
