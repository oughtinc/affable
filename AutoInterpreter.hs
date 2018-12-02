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
import Data.Text.Lazy.Builder.Int ( decimal ) -- text
import Data.Traversable ( traverse ) -- base

import Message ( Message(..), Pointer, PointerRemapping, messageToBuilder, matchMessage,
                 matchPointers, expandPointers, substitute, renumberMessage' )
import Scheduler ( Event(..), SchedulerContext(..), SchedulerFn )
import Workspace ( WorkspaceId, Workspace(..), emptyWorkspace )
import Util ( toText, invertMap )

type Value = Message
type Pattern = Message

-- TODO: Make data model and figure out how to serialize to database.
-- NOTE: Currently only ever use LetFun f (Call f _) pattern which is essentially, unsurprisingly,
-- equivalent to doing a case analysis on _. It's possible having Call be separate could be useful
-- for "tail calls".
data Exp f v
    = Var v                         -- x
    | Value Value                   -- Structured [Text "foo", Reference 1, Reference 2]
    | Call f (Exp f v)              -- f(e)
    | LetFun f (Exp f v)            -- let f(Structured [Text "foo", p1, p2]) = e1 -- The cases arise magically, i.e. via lookup indexed by f.
  deriving ( Read, Show )           --     f(Structured [Text "bar", p1]) = e2
                                    --     f(p1) = e3
                                    -- in e4

expToBuilder' :: (f -> Builder) -> (v -> Builder) -> (f -> [(Pattern, Exp f v)]) -> Exp f v -> Builder
expToBuilder' fBuilder vBuilder alternativesFor = go 0
    where go indent (Var x) = vBuilder x
          go indent (Value v) = valueToBuilder v
          go indent (Call f e) = fBuilder f <> singleton '(' <> go 0 e <> singleton ')'
          {-
          go indent (LetFun f body) | null alts = fromText "let "
                                               <> indentBuilder <> fromText "  " <> fBuilder f <> fromText "(_) = undefined"
                                               <> indentBuilder <> fromText "in " <> go indent body
                                    | otherwise = fromText "let"
                                               <> foldMap (\(p, e) -> f' p <> fromText " = " <> go (indent + 4) e) alts
                                               <> indentBuilder <> fromText "in " <> go indent body
          -}
          go indent (LetFun f body) | null alts = go indent body <> fromText " where "
                                               <> indentBuilder <> fromText "  " <> fBuilder f <> fromText "(_) = undefined"
                                    | otherwise = go indent body <> fromText " where "
                                               <> foldMap (\(p, e) -> f' p <> fromText " = " <> go (indent + 2) e) alts
            where !indentBuilder = singleton '\n' <> fromText (T.replicate indent " ")
                  !alts = alternativesFor f
                  f' p = indentBuilder <> fromText "  " <> fBuilder f <> singleton '(' <> valueToBuilder p <> singleton ')'
          valueToBuilder = messageToBuilder -- TODO: Or use show or something else?

expToBuilder :: (Name -> [(Pattern, Exp Name Var)]) -> Exp Name Var -> Builder
expToBuilder = expToBuilder' nameToBuilder (\v -> singleton '$' <> decimal v)

type VarEnv v = M.Map v Value
type VarMapping v = M.Map v v
type FunEnv s m f = M.Map f (s -> Value -> m Value)

-- NOTE: We could do a "parallel" evaluator that might allow multiple workspaces to be scheduled.
evaluateExp :: (Ord f, Ord v, Monad m)
            => (s -> VarEnv v -> f -> Value -> m (s, VarEnv v, Exp f v))
            -> (VarEnv v -> Value -> Value)
            -> s
            -> Exp f v
            -> m Value
evaluateExp match subst = evaluateExp' match subst M.empty M.empty

evaluateExp' :: (Ord f, Ord v, Monad m)
             => (s -> VarEnv v -> f -> Value -> m (s, VarEnv v, Exp f v))
             -> (VarEnv v -> Value -> Value)
             -> VarEnv v
             -> FunEnv s m f
             -> s
             -> Exp f v
             -> m Value
evaluateExp' match subst = go
    where go varEnv funEnv s (Var x) = return $! case M.lookup x varEnv of Just v -> v
          go varEnv funEnv s (Value v) = return $ subst varEnv v
          go varEnv funEnv s (Call f e) = do
            v <- go varEnv funEnv s e -- NOTE: This is call-by-value. It may be worth experimenting with call-by-name.
            (case M.lookup f funEnv of Just f' -> f') s v
          go varEnv funEnv s (LetFun f body) = do
            let fEvaled s v = do
                    (s', varEnv', e) <- match s varEnv f v
                    -- go varEnv' funEnv s' e -- non-recursive let
                    go varEnv' funEnv' s' e -- recursive let
                funEnv' = M.insert f fEvaled funEnv
            go varEnv funEnv' s body

data Name = ANSWER | LOCAL !Int deriving ( Eq, Ord, Show )

nameToBuilder :: Name -> Builder
nameToBuilder ANSWER = fromText "answer"
nameToBuilder (LOCAL i) = singleton 'f' <> decimal i

type Var = Pointer

type Exp' = Exp Name Var

relabelMessage ctxt (LabeledStructured _ ms) = labelMessage ctxt (Structured ms)
relabelMessage ctxt msg = labelMessage ctxt msg

-- NOTE: Instead of using forkIO and co, we could use a monad other than IO for
-- expression evaluation that supports suspending a computation or implements cooperative
-- concurrency.
makeInterpreterScheduler :: SchedulerContext extra -> WorkspaceId -> IO SchedulerFn
makeInterpreterScheduler ctxt initWorkspaceId = do
    alternativesRef <- newIORef (M.empty :: M.Map Name [(Pattern, Exp')]) -- TODO: Load from database.

    workspaceVariablesRef <- newIORef (M.empty :: M.Map WorkspaceId PointerRemapping) -- TODO: Store in database(?) Maybe not?

    -- Hacky? Holds pointers to the answers to the latest pending questions for workspaces, if any.
    -- This strongly suggests a sequential workflow, which isn't wrong, but isn't desirable either.
    -- For a more parallel workflow, we could identify subquestions and have a mapping from workspaces to subquestions.
    answersRef <- newIORef (M.empty :: M.Map WorkspaceId Message) -- TODO: This is just the answer already in the database. Though this does indicate the need to look.

    idRef <- newIORef (0 :: Int) -- When the alternatives are in the database, this will effectively become a database ID.

    requestMVar <- newEmptyMVar :: IO (MVar (Maybe WorkspaceId))
    responseMVar <- newEmptyMVar :: IO (MVar Event)

    let genSym = atomicModifyIORef' idRef (\n -> (n+1, LOCAL n))

        linkVars workspaceId mapping = modifyIORef' workspaceVariablesRef $ M.insertWith M.union workspaceId mapping
        links workspaceId = (maybe M.empty id . M.lookup workspaceId) <$> readIORef workspaceVariablesRef

        giveAnswer workspaceId p = modifyIORef' answersRef $ M.insert workspaceId p
        retrieveAnswer workspaceId = atomicModifyIORef' answersRef ((\(x,y) -> (y,x)) . M.updateLookupWithKey (\_ _ -> Nothing) workspaceId)

        debugCode = do
            altMap <- readIORef alternativesRef
            T.putStrLn (toText (expToBuilder (\f -> maybe [] reverse $ M.lookup f altMap) (LetFun ANSWER (Value (Text "dummy")))))

        blockOnUser !mWorkspace = do
            putMVar requestMVar mWorkspace
            takeMVar responseMVar

        replyFromUser e = do
            putMVar responseMVar e
            takeMVar requestMVar

        match s varEnv f (Reference p) = do
            m <- dereference ctxt p
            match s varEnv f m
        match workspaceId varEnv f m = do
            workspace <- getWorkspace ctxt workspaceId
            altsMap <- readIORef alternativesRef
            case M.lookup f altsMap of -- TODO: Could mark workspaces as "human-influenced" when a pattern match failure is hit
                                       -- or when any subquestions are marked. This would allow "garbage collecting" workspaces
                                       -- with answers that are not "human-influenced", i.e. were created entirely through automation.
                Just alts -> do
                    m <- normalize ctxt m
                    let !mMatch = asum $ map (\(p, e) -> fmap (\bindings -> (p, M.union varEnv bindings, e)) $ matchMessage p m) alts
                    case mMatch of
                        Just (pattern, varEnv', e) -> do
                            varEnv' <- traverse (\m -> case m of Reference p -> dereference ctxt p; _ -> return m) varEnv'
                            -- This is to make it so occurrences of variables bound by as-patterns don't get substituted.
                            -- This leads to match being called on References if we reply with the variable bound by an as-pattern.
                            bindings <- case (pattern, m) of
                                            (LabeledStructured asP _, LabeledStructured l _) -> return $ M.insert asP (Reference l) varEnv'
                                            _ -> return varEnv'

                            (mapping, child) <- case f of
                                                        ANSWER -> do
                                                            (mapping, pattern) <- instantiate ctxt bindings pattern
                                                            -- TODO: Have a separate "as asked" and "as answered" question field
                                                            -- for workspaces? Otherwise, if you ask "foo $1 $1", the question you'll
                                                            -- seen, once answered, is "foo $2 $3" because that's the question that
                                                            -- was answered.
                                                            newWorkspaceId <- createWorkspace ctxt False workspace pattern
                                                            fmap ((,) mapping) $ getWorkspace ctxt newWorkspaceId
                                                        _ -> return (M.empty, workspace)
                            let !childId = identity child
                            let !invMapping = invertMap mapping
                            linkVars childId mapping
                            -- This is a bit hacky. If this approach is the way to go, make these patterns individual constructors.
                            -- I'd also prefer a design that only created workspace when necessary. I envision something that executes
                            -- the automation creating nothing if there are no pattern match failures. If there is a pattern match failure,
                            -- this will create a new workspace that will lead to the creation (via functional updating) of new workspace
                            -- as the change percolates back up the tree of questions.
                            case e of
                                LetFun _ (Call _ (Call ANSWER (Value msg))) -> do -- ask case
                                    return (childId, varEnv', e)
                                LetFun _ (Call _ (Var ptr)) -> do -- expand case
                                    -- TODO: Add links here too?
                                    expandPointer ctxt child $! maybe ptr id $ M.lookup ptr invMapping
                                    return (childId, varEnv', e)
                                Value msg -> do -- reply case
                                    let varEnv'' = varEnv'
                                    let !msg' = substitute bindings msg
                                    msg <- relabelMessage ctxt msg'
                                    sendAnswer ctxt False child msg
                                    case parentId workspace of Just pId -> giveAnswer pId msg; _ -> return ()
                                    return (childId, varEnv'', e)
                                -- Just _ -> return (workspaceId, varEnv', e) -- Intentionally missing this case.
                        Nothing -> matchFailed workspace
                Nothing -> matchFailed workspace
            where matchFailed workspace = do
                    m' <- normalize ctxt m
                    pattern <- generalize ctxt m' -- NOTE: If we want pointers to questions, label this pattern.
                    pattern@(LabeledStructured asP _) <- relabelMessage ctxt pattern
                    workspace <- case f of
                                    ANSWER -> do
                                        newWorkspaceId <- createWorkspace ctxt False workspace pattern
                                        getWorkspace ctxt newWorkspaceId
                                    _ -> return workspace
                    let !workspaceId = identity workspace

                    let !(Just bindings) = matchMessage pattern m' -- This shouldn't fail.
                    bindings <- traverse (\m -> case m of Reference p -> dereference ctxt p; _ -> return m) bindings
                    let !varEnv' = M.union varEnv bindings

                    mAnswer <- retrieveAnswer workspaceId
                    case mAnswer of
                        Just a -> linkVars workspaceId $ matchPointers pattern a
                        Nothing -> return ()

                    globalToLocal <- links workspaceId
                    evt <- blockOnUser (Just workspaceId)
                    e <- case evt of
                            Create msg -> do
                                g <- genSym
                                return $ LetFun g (Call g (Call ANSWER (Value $ renumberMessage' globalToLocal msg)))
                            Expand ptr -> do
                                -- TODO: When we expand pointers, we need to add links.
                                expandPointer ctxt workspace ptr
                                g <- genSym
                                let !ptr' = maybe ptr id $ M.lookup ptr globalToLocal
                                return $ LetFun g (Call g (Var ptr'))
                            Answer msg -> do
                                msg' <- relabelMessage ctxt msg
                                sendAnswer ctxt False workspace msg'
                                case parentId workspace of Just pId -> giveAnswer pId msg'; _ -> return ()
                                return $ Value $ renumberMessage' globalToLocal msg
                            -- Send ws msg -> Intentional.
                    modifyIORef' alternativesRef (M.insertWith (++) f [(pattern, e)])
                    debugCode
                    return (workspaceId, varEnv', e)

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
        t <- evaluateExp' match substitute M.empty M.empty initWorkspaceId startExp
        T.putStrLn (toText (messageToBuilder t))
        blockOnUser Nothing
        return ()

    return scheduler
