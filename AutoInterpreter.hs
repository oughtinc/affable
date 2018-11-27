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
import Util ( toText )

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
          go indent (Call f e) = fBuilder f <> {-singleton ':' <> decimal ws <>-} singleton '(' <> go 0 e <> singleton ')'
          go indent (LetFun f body) | null alts = fromText "let "
                                               <> indentBuilder <> fromText "  " <> fBuilder f <> fromText "(_) = undefined"
                                               <> indentBuilder <> fromText "in " <> go indent body
                                    | otherwise = fromText "let"
                                               <> foldMap (\(p, e) -> f' p <> fromText " = " <> go (indent + 4) e) alts
                                               <> indentBuilder <> fromText "in " <> go indent body
            where !indentBuilder = singleton '\n' <> fromText (T.replicate indent " ")
                  !alts = alternativesFor f
                  f' p = indentBuilder <> fromText "  " <> fBuilder f <> singleton '(' <> valueToBuilder p <> singleton ')'
          valueToBuilder = messageToBuilder -- TODO: Or use show or something else?

expToBuilder :: (Name -> [(Message, Exp Name Var)]) -> Exp Name Var -> Builder
expToBuilder = expToBuilder' nameToBuilder (\v -> singleton '$' <> decimal v)

type VarEnv v = M.Map v Value
type FunEnv m f = M.Map f (WorkspaceId -> Value -> m Value)

-- NOTE: We could do a "parallel" evaluator that might allow multiple workspaces to be scheduled.
evaluateExp :: (Ord f, Ord v, Monad m)
            => (VarEnv v -> WorkspaceId -> f -> Message -> m (VarEnv v, WorkspaceId, Exp f v))
            -> (VarEnv v -> Value -> Value)
            -> WorkspaceId
            -> Exp f v
            -> m Value
evaluateExp match subst = evaluateExp' match subst M.empty M.empty

evaluateExp' :: (Ord f, Ord v, Monad m)
             => (VarEnv v -> WorkspaceId -> f -> Message -> m (VarEnv v, WorkspaceId, Exp f v))
             -> (VarEnv v -> Value -> Value)
             -> VarEnv v
             -> FunEnv m f
             -> WorkspaceId
             -> Exp f v
             -> m Value
{-
evaluateExp' :: (VarEnv Var -> WorkspaceId -> Name -> Message -> IO (VarEnv Var, WorkspaceId, Exp Name Var))
             -> (VarEnv Var -> Value -> Value)
             -> VarEnv Var
             -> FunEnv IO Name
             -> WorkspaceId
             -> Exp Name Var
             -> IO Value
-}
evaluateExp' match subst = go
    where go varEnv funEnv ws (Var x) = return $! case M.lookup x varEnv of Just v -> v
          go varEnv funEnv ws (Value v) = return $ subst varEnv v
          go varEnv funEnv ws (Call f e) = do
            v <- go varEnv funEnv ws e
            (case M.lookup f funEnv of Just f' -> f') ws v
          go varEnv funEnv ws (LetFun f body) = do
            let fEvaled ws v = do
                    (varEnv', ws', e) <- match varEnv ws f v
                    -- go varEnv' funEnv ws' e -- non-recursive let
                    go varEnv' funEnv' ws' e -- recursive let
                funEnv' = M.insert f fEvaled funEnv
            go varEnv funEnv' ws body

data Name = ANSWER | LOCAL !Int deriving ( Eq, Ord, Show )

nameToBuilder :: Name -> Builder
nameToBuilder ANSWER = fromText "answer"
nameToBuilder (LOCAL i) = singleton 'f' <> decimal i

type Var = Pointer

type Exp' = Exp Name Var

-- NOTE: Instead of using forkIO and co, we could use a monad other than IO for
-- expression evaluation that supports suspending a computation or implements cooperative
-- concurrency.
makeInterpreterScheduler :: SchedulerContext extra -> IO SchedulerFn
makeInterpreterScheduler ctxt = do
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

        match varEnv workspaceId f (Reference p) = do -- TODO: This is what is desired?
            m <- dereference ctxt p
            match varEnv workspaceId f m -- TODO: Probably want to do some renumbering. Wrap the result in a Structured?
        match varEnv workspaceId f m = do
            workspace <- getWorkspace ctxt workspaceId
            altsMap <- readIORef alternativesRef
            case M.lookup f altsMap of
                Just alts -> do
                    let !mMatch = asum $ map (\(p, e) -> fmap (\bindings -> (p, M.union varEnv bindings, e)) $ matchMessage p m) alts
                    -- This is a bit hacky. If this approach is the way to go, make these patterns individual constructors.
                    -- I'd also prefer a design that only created workspace when necessary. I envision something that executes
                    -- the automation creating nothing if there are no pattern match failures. If there is a pattern match failure,
                    -- this will create a new workspace that will lead to the creation (via functional updating) of new workspace
                    -- as the change percolates back up the tree of questions.
                    case mMatch of
                        Just (p, varEnv', e) -> do
                            workspace <- case f of
                                            ANSWER -> do
                                                newWorkspaceId <- createWorkspace ctxt workspace p
                                                getWorkspace ctxt newWorkspaceId
                                            _ -> return workspace
                            let !workspaceId = identity workspace
                            case e of
                                LetFun _ (Call _ (Call ANSWER (Value _))) -> do -- ask case
                                    return (varEnv', workspaceId, e)
                                LetFun _ (Call _ (Var ptr)) -> do -- expand case
                                    expandPointer ctxt workspace ptr
                                    return (varEnv', workspaceId, e)
                                Value msg -> do -- reply case
                                    sendAnswer ctxt workspace $! expandPointers varEnv' msg -- TODO: This right?
                                    return (varEnv', workspaceId {- Doesn't really matter -}, e)
                                -- Just _ -> return (varEnv', workspace, e) -- Intentionally missing this case.
                        Nothing -> matchFailed workspace
                Nothing -> matchFailed workspace
            where matchFailed workspace = do
                    pattern <- generalize ctxt m
                    workspace <- case f of
                                    ANSWER -> do
                                        newWorkspaceId <- createWorkspace ctxt workspace pattern
                                        getWorkspace ctxt newWorkspaceId
                                    _ -> return workspace
                    let !workspaceId = identity workspace
                    evt <- blockOnUser (Just workspaceId)
                    e <- case evt of
                            Create msg -> do
                                msg <- normalize ctxt msg
                                g <- genSym
                                return $ LetFun g (Call g (Call answerFn (Value msg)))
                            Expand ptr -> do
                                expandPointer ctxt workspace ptr
                                g <- genSym
                                return $ LetFun g (Call g (Var ptr)) -- TODO: ptr needs to correspond to a "variable"
                            Answer msg -> do
                                msg <- normalize ctxt msg
                                sendAnswer ctxt workspace msg
                                return $ Value msg
                            -- Send ws msg -> Intentional.
                    modifyIORef' alternativesRef (M.insertWith (++) f [(pattern, e)])
                    debugCode
                    let !(Just bindings) = matchMessage pattern m -- This shouldn't fail.
                    let varEnv' = M.union varEnv bindings
                    return (varEnv', workspaceId, e)


        scheduler user workspace (Send ws msg) = do
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
        t <- evaluateExp' match expandPointers M.empty M.empty 0 startExp
        T.putStrLn (toText (messageToBuilder t))
        blockOnUser Nothing
        return ()

    return scheduler
