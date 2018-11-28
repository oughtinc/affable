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

import Message ( Message(..), Pointer, messageToBuilder, matchMessage, expandPointers )
import Scheduler ( Event(..), SchedulerContext(..), SchedulerFn )
import Workspace ( WorkspaceId, Workspace(..), emptyWorkspace )
import Util ( toText, invertMap )

type Value = Message

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

expToBuilder' :: (f -> Builder) -> (v -> Builder) -> (f -> [(Message, Exp f v)]) -> Exp f v -> Builder
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

expToBuilder :: (Name -> [(Message, Exp Name Var)]) -> Exp Name Var -> Builder
expToBuilder = expToBuilder' nameToBuilder (\v -> singleton '$' <> decimal v)

type VarEnv v = M.Map v Value
type VarMapping v = M.Map v v
type FunEnv s m f = M.Map f (s -> Value -> m Value)

-- NOTE: We could do a "parallel" evaluator that might allow multiple workspaces to be scheduled.
evaluateExp :: (Ord f, Ord v, Monad m)
            => (s -> VarEnv v -> f -> Message -> m (s, VarEnv v, Exp f v))
            -> (VarEnv v -> Value -> Value)
            -> s
            -> Exp f v
            -> m Value
evaluateExp match subst = evaluateExp' match subst M.empty M.empty

evaluateExp' :: (Ord f, Ord v, Monad m)
             => (s -> VarEnv v -> f -> Message -> m (s, VarEnv v, Exp f v))
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

-- NOTE: Instead of using forkIO and co, we could use a monad other than IO for
-- expression evaluation that supports suspending a computation or implements cooperative
-- concurrency.
makeInterpreterScheduler :: SchedulerContext extra -> WorkspaceId -> IO SchedulerFn
makeInterpreterScheduler ctxt initWorkspaceId = do
    let answerFn = ANSWER
    alternativesRef <- newIORef (M.empty :: M.Map Name [(Message, Exp')]) -- TODO: Load from database.
    idRef <- newIORef (0 :: Int)

    requestMVar <- newEmptyMVar :: IO (MVar (Maybe WorkspaceId))
    responseMVar <- newEmptyMVar :: IO (MVar Event)

    let genSym = atomicModifyIORef' idRef (\n -> (n+1, LOCAL n))

        debugCode = do
            altMap <- readIORef alternativesRef
            T.putStrLn (toText (expToBuilder (\f -> maybe [] reverse $ M.lookup f altMap) (LetFun answerFn (Value (Text "dummy")))))

        blockOnUser !mWorkspace = do
            putMVar requestMVar mWorkspace
            takeMVar responseMVar

        replyFromUser e = do
            putMVar responseMVar e
            takeMVar requestMVar

        match s varEnv f (Reference p) = do -- TODO: This is what is desired?
            m <- dereference ctxt p
            match s varEnv f m -- TODO: Wrap the result in a Structured?
        match (globalToLocal, workspaceId) varEnv f m = do
            workspace <- getWorkspace ctxt workspaceId
            altsMap <- readIORef alternativesRef
            case M.lookup f altsMap of -- TODO: Could mark workspaces as "human-influenced" when a pattern match failure is hit
                                       -- or when any subquestions are marked. This would allow "garbage collecting" workspaces
                                       -- with answers that are not "human-influenced", i.e. were created entirely through automation.
                Just alts -> do
                    let !mMatch = asum $ map (\(p, e) -> fmap (\bindings -> (p, M.union varEnv bindings, e)) $ matchMessage p m) alts
                    case mMatch of
                        Just (pattern, varEnv', e) -> do
                            (mapping, workspace) <- case f of
                                                        ANSWER -> do
                                                            (mapping, pattern) <- instantiate ctxt varEnv' pattern
                                                            newWorkspaceId <- createWorkspace ctxt workspace pattern
                                                            fmap ((,) mapping) $ getWorkspace ctxt newWorkspaceId
                                                        _ -> return (M.empty, workspace)
                            let !workspaceId = identity workspace
                            let !invMapping = invertMap mapping
                            let !globalToLocal' = M.union mapping globalToLocal
                            -- This is a bit hacky. If this approach is the way to go, make these patterns individual constructors.
                            -- I'd also prefer a design that only created workspace when necessary. I envision something that executes
                            -- the automation creating nothing if there are no pattern match failures. If there is a pattern match failure,
                            -- this will create a new workspace that will lead to the creation (via functional updating) of new workspace
                            -- as the change percolates back up the tree of questions.
                            case e of
                                LetFun _ (Call _ (Call ANSWER (Value _))) -> do -- ask case
                                    return ((globalToLocal', workspaceId), varEnv', e)
                                LetFun _ (Call _ (Var ptr)) -> do -- expand case
                                    expandPointer ctxt workspace $! maybe ptr id $ M.lookup ptr invMapping
                                    return ((globalToLocal', workspaceId), varEnv', e)
                                Value msg -> do -- reply case
                                    sendAnswer ctxt workspace $! expandPointers varEnv' msg -- TODO: This right?
                                    return ((globalToLocal', workspaceId), varEnv', e)
                                -- Just _ -> return ((globalToLocal', workspaceId), varEnv', e) -- Intentionally missing this case.
                        Nothing -> matchFailed workspace
                Nothing -> matchFailed workspace
            where matchFailed workspace = do
                    pattern <- generalize ctxt =<< normalize ctxt m
                    workspace <- case f of
                                    ANSWER -> do
                                        newWorkspaceId <- createWorkspace ctxt workspace pattern
                                        getWorkspace ctxt newWorkspaceId
                                    _ -> return workspace
                    let !workspaceId = identity workspace
                    evt <- blockOnUser (Just workspaceId)
                    e <- case evt of
                            Create msg -> do
                                g <- genSym
                                return $ LetFun g (Call g (Call answerFn (Value msg)))
                            Expand ptr -> do
                                let !ptr' = maybe ptr id $ M.lookup ptr globalToLocal
                                expandPointer ctxt workspace ptr
                                g <- genSym
                                return $ LetFun g (Call g (Var ptr'))
                            Answer msg -> do
                                sendAnswer ctxt workspace $! expandPointers varEnv msg
                                return $ Value msg
                            -- Send ws msg -> Intentional.
                    modifyIORef' alternativesRef (M.insertWith (++) f [(pattern, e)])
                    debugCode
                    let !(Just bindings) = matchMessage pattern m -- This shouldn't fail.
                    let varEnv' = M.union varEnv bindings
                    return ((globalToLocal, workspaceId), varEnv', e)

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
        let startExp = LetFun answerFn (Call answerFn (Value msg))
        t <- evaluateExp' match expandPointers M.empty M.empty (M.empty, initWorkspaceId) startExp
        T.putStrLn (toText (messageToBuilder t))
        blockOnUser Nothing
        return ()

    return scheduler
