{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module AutoInterpreter where
import Control.Concurrent ( forkIO ) -- base
import Control.Concurrent.MVar ( MVar, newEmptyMVar, newMVar, putMVar, takeMVar ) -- base
import Data.Foldable ( asum ) -- base
import Data.IORef ( IORef, newIORef, readIORef, writeIORef, atomicModifyIORef', modifyIORef' ) -- base
import qualified Data.Map as M -- containers
import Data.String ( fromString ) -- base
import Data.Text ( Text ) -- text
import qualified Data.Text as T -- text
import qualified Data.Text.IO as T -- text
import Data.Text.Lazy.Builder ( Builder, singleton, fromText ) -- text
import Data.Traversable ( traverse ) -- base

import AutoScheduler ( AutoSchedulerContext(..) )
import Exp ( Exp(..), Exp', Name(..), Var, Value, Pattern, evaluateExp', expToBuilder, expToHaskell )
import Message ( Message(..), Pointer, PointerRemapping, messageToBuilder, matchMessage,
                 matchPointers, expandPointers, substitute, renumberMessage' )
import Primitive ( makePrimitives )
import Scheduler ( Event(..), SchedulerContext(..), SchedulerFn, relabelMessage, fullyExpand )
import Util ( toText, invertMap )
import Workspace ( WorkspaceId, Workspace(..), emptyWorkspace )

-- NOTE: Instead of using forkIO and co, we could use a monad other than IO for
-- expression evaluation that supports suspending a computation or implements cooperative
-- concurrency.
makeInterpreterScheduler :: AutoSchedulerContext extra -> WorkspaceId -> IO SchedulerFn
makeInterpreterScheduler autoCtxt initWorkspaceId = do
    let !ctxt = schedulerContext autoCtxt

    -- Mapping of pointers from replay workspace to original workspace.
    workspaceVariablesRef <- newIORef (M.empty :: M.Map WorkspaceId PointerRemapping) -- TODO: Store in database(?) Maybe not?

    -- Hacky? Holds pointers to the answers to the latest pending questions for workspaces, if any.
    -- This strongly suggests a sequential workflow, which isn't wrong, but isn't desirable either.
    -- For a more parallel workflow, we could identify subquestions and have a mapping from workspaces to subquestions.
    answersRef <- newIORef (M.empty :: M.Map WorkspaceId Message) -- TODO: This is just the answer already in the database. Though this does indicate the need to look.

    requestMVar <- newEmptyMVar :: IO (MVar (Maybe WorkspaceId))
    responseMVar <- newEmptyMVar :: IO (MVar Event)

    let linkVars workspaceId mapping = modifyIORef' workspaceVariablesRef $ M.insertWith M.union workspaceId mapping
        links workspaceId = (maybe M.empty id . M.lookup workspaceId) <$> readIORef workspaceVariablesRef

        giveArgument workspaceId p = modifyIORef' answersRef $ M.insert workspaceId p
        retrieveArgument workspaceId = atomicModifyIORef' answersRef ((\(x,y) -> (y,x)) . M.updateLookupWithKey (\_ _ -> Nothing) workspaceId)

        debugCode = do
            altMap <- allAlternatives autoCtxt
            -- T.putStrLn (toText (expToHaskell (\f -> maybe [] reverse $ M.lookup f altMap) (LetFun ANSWER (Value (Text "dummy")))))
            T.putStrLn (toText (expToBuilder (\f -> maybe [] reverse $ M.lookup f altMap) (LetFun ANSWER (Value (Text "dummy")))))

        blockOnUser !mWorkspace = do
            putMVar requestMVar mWorkspace
            takeMVar responseMVar

        replyFromUser evt = do
            putMVar responseMVar evt
            takeMVar requestMVar

    (primEnv, matchPrim) <- makePrimitives ctxt giveArgument

    let match s varEnv f m@(Reference p) = do
            m <- dereference ctxt p
            match s varEnv f m
        match workspaceId varEnv f m = do
            workspace <- getWorkspace ctxt workspaceId
            alts <- alternativesFor autoCtxt f
            m' <- normalize ctxt =<< generalize ctxt m
            case alts of -- TODO: Could mark workspaces as "human-influenced" when a pattern match failure is hit
                         -- or when any subquestions are marked. This would allow "garbage collecting" workspaces
                         -- with answers that are not "human-influenced", i.e. were created entirely through automation.
                _:_ -> do
                    -- TODO: Error out if there is more than one match.
                    let !mMatch = asum $ map (\(p, e) -> fmap (\bindings -> (p, M.union bindings varEnv, e)) $ matchMessage p m') alts
                    case mMatch of
                        Just (pattern, varEnv', e) -> do
                            -- This is to make it so occurrences of variables bound by as-patterns don't get substituted.
                            -- This leads to match being called on References if we reply with the variable bound by an as-pattern.
                            bindings <- case (pattern, m') of
                                            (LabeledStructured asP _, LabeledStructured l _) -> return $ M.insert asP (Reference l) varEnv'
                                            _ -> return varEnv'

                            (invMapping, childId) <- case f of
                                                        ANSWER -> do
                                                            (mapping, pattern) <- instantiate ctxt bindings pattern
                                                            let !invMapping = invertMap mapping
                                                            -- To make sure the pattern in createWorkspace is appropriately expanded.
                                                            -- TODO: There's probably a better way to do this.
                                                            pattern <- case e of
                                                                            LetFun _ (Call _ (Var ptr)) -> do
                                                                                let !(Just p') = M.lookup ptr invMapping
                                                                                let !(Just (Reference p)) = M.lookup ptr bindings -- TODO: Will fail if already expanded.
                                                                                arg <- dereference ctxt p
                                                                                return $! expandPointers (M.singleton p' arg) pattern
                                                                            _ -> return pattern
                                                            newWorkspaceId <- createWorkspace ctxt False workspaceId m pattern
                                                            linkVars newWorkspaceId mapping
                                                            return (invMapping, newWorkspaceId)
                                                        _ -> return (M.empty, workspaceId)

                            mAnswer <- retrieveArgument workspaceId
                            case mAnswer of
                                Just a -> linkVars childId $ matchPointers pattern a
                                Nothing -> return ()

                            invMapping <- links childId
                            -- This is a bit hacky. If this approach is the way to go, make these patterns individual constructors.
                            -- I'd also prefer a design that only created workspace when necessary. I envision something that executes
                            -- the automation creating nothing if there are no pattern match failures. If there is a pattern match failure,
                            -- this will create a new workspace that will lead to the creation (via functional updating) of new workspace
                            -- as the change percolates back up the tree of questions.
                            case e of
                                LetFun _ (Call _ (Call ANSWER (Value _))) -> do -- ask case
                                    return (childId, varEnv', e)
                                LetFun _ (Call _ (Prim _ (Value _))) -> do -- ask prim case
                                    return (childId, varEnv', e)
                                LetFun _ (Call _ (Var ptr)) -> do -- expand case
                                    let !ptr' = maybe ptr id $ M.lookup ptr invMapping
                                    -- TODO: This is somewhat duplicated in the ANSWER branch above.
                                    let !(Just (Reference p)) = M.lookup ptr bindings -- TODO: Will fail if already expanded.
                                    arg <- dereference ctxt p
                                    expandPointer ctxt childId p -- ptr'
                                    giveArgument childId arg
                                    return (childId, M.insert ptr arg varEnv', e)
                                Value msg -> do -- reply case
                                    let !msg' = substitute bindings msg
                                    msg <- normalize ctxt =<< case msg' of Reference p -> dereference ctxt p; _ -> relabelMessage ctxt msg'
                                    sendAnswer ctxt False childId msg
                                    case parentId workspace of Just pId -> giveArgument pId msg; _ -> return ()
                                    return (childId, varEnv', e)
                                -- Just _ -> return (workspaceId, varEnv', e) -- Intentionally missing this case.
                        Nothing -> matchFailed workspace m'
                [] -> matchFailed workspace m'
            where matchFailed workspace m' = do
                    pattern <- generalize ctxt m' -- NOTE: If we want pointers to questions, label this pattern.
                    pattern@(LabeledStructured asP _) <- relabelMessage ctxt pattern
                    workspace <- case f of
                                    ANSWER -> do
                                        newWorkspaceId <- createWorkspace ctxt False workspaceId m pattern
                                        getWorkspace ctxt newWorkspaceId
                                    _ -> return workspace
                    let !workspaceId = identity workspace

                    let !(Just bindings) = matchMessage pattern m' -- This shouldn't fail.
                    let !varEnv' = M.union bindings varEnv

                    mAnswer <- retrieveArgument workspaceId
                    case mAnswer of
                        Just a -> linkVars workspaceId $ matchPointers pattern a
                        Nothing -> return ()

                    globalToLocal <- links workspaceId
                    evt <- blockOnUser (Just workspaceId)
                    let processEvent (Create msg) = do
                            g <- newFunction autoCtxt
                            case matchPrim msg of
                                Just p -> return (M.empty, LetFun g (Call g (Prim p (Value $ renumberMessage' globalToLocal msg))))
                                Nothing -> return (M.empty, LetFun g (Call g (Call ANSWER (Value $ renumberMessage' globalToLocal msg))))
                        processEvent (Expand ptr) = do
                            expandPointer ctxt workspaceId ptr
                            arg <- dereference ctxt ptr
                            giveArgument workspaceId arg
                            g <- newFunction autoCtxt
                            let !ptr' = maybe ptr id $ M.lookup ptr globalToLocal
                            return (M.singleton ptr' arg, LetFun g (Call g (Var ptr')))
                        processEvent (Answer msg@(Structured [Reference p])) = do -- dereference pointers -- TODO: Do this?
                            msg' <- dereference ctxt p
                            sendAnswer ctxt False workspaceId msg'
                            case parentId workspace of Just pId -> giveArgument pId msg'; _ -> return ()
                            return (M.empty, Value $ renumberMessage' globalToLocal msg)
                        processEvent (Answer msg) = do
                            msg' <- relabelMessage ctxt =<< normalize ctxt msg
                            sendAnswer ctxt False workspaceId msg'
                            case parentId workspace of Just pId -> giveArgument pId msg'; _ -> return ()
                            return (M.empty, Value $ renumberMessage' globalToLocal msg)
                        -- Send ws msg -> Intentional.
                    (extraBindings, e) <- processEvent evt
                    addCaseFor autoCtxt f pattern e
                    debugCode
                    return (workspaceId, M.union extraBindings varEnv', e)

        scheduler _ workspace (Send ws msg) = do
            -- TODO: Think about this and support it if it makes sense.
            -- sendMessage ctxt workspace ws msg
            putStrLn "makeInterpreterScheduler: Message sending not supported."
            Just <$> getWorkspace ctxt (identity workspace)
        scheduler _ _ evt = do
            mWorkspaceId <- replyFromUser evt
            case mWorkspaceId of
                Just workspaceId -> Just <$> getWorkspace ctxt workspaceId
                Nothing -> return Nothing

    forkIO $ do
        Create msg <- takeMVar responseMVar -- TODO: Better error handling.
        let startExp = LetFun ANSWER (Call ANSWER (Value msg)) :: Exp'
        t <- evaluateExp' match substitute primEnv M.empty M.empty initWorkspaceId startExp
        t <- fullyExpand ctxt t
        T.putStrLn (toText (messageToBuilder t))
        blockOnUser Nothing
        return ()

    return scheduler
