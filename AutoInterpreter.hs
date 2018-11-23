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

import Message ( Message(..), Pointer, messageToBuilder, matchMessage )
import Scheduler ( Event(..), SchedulerContext(..), SchedulerFn )
import Workspace ( WorkspaceId, Workspace(..) )
import Util ( toText )

type Value = Message

-- Have <loc> in Undefined <loc> be a "pointer"/key for another Exp if it exists. If it doesn't exist,
-- evaluating this will lead to the creation of a new workspace.

-- TODO: Make data model and figure out how to serialize to database.
-- NOTE: Currently only ever use LetFun f (Call f _) pattern which is essentially, unsurprisingly,
-- equivalent to doing a case analysis on _. It's possible having Call be separate could be useful
-- for "tail calls".
data Exp f v
    = Var v                         -- x
    | Value Value                   -- Structured [Text "foo", Reference 1, Reference 2]
    | Call WorkspaceId f (Exp f v)  -- f(e)
    | LetFun f (Exp f v)            -- let f(Structured [Text "foo", p1, p2]) = e1 -- The cases arise magically, i.e. via lookup indexed by f.
  deriving ( Read, Show )           --     f(Structured [Text "bar", p1]) = e2
                                    --     f(p1) = e3
                                    -- in e4

expToBuilder' :: (f -> Builder) -> (v -> Builder) -> (f -> [(Message, Exp f v)]) -> Exp f v -> Builder
expToBuilder' fBuilder vBuilder alternativesFor = go 0
    where go indent (Var x) = vBuilder x
          go indent (Value v) = valueToBuilder v
          go indent (Call ws f e) = fBuilder f <> singleton ':' <> decimal ws <> singleton '(' <> go 0 e <> singleton ')'
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

{-
exampleExp :: (Exp Text Int, Text -> [(Message, Exp Text Int)])
exampleExp = (LetFun "f" (Call "f" (Undefined 1)), (alts M.!))
    where alts = M.fromList [("f", [
                                (Structured [Text "foo ", Reference 1], Undefined 2),
                                (Structured [Text "bar ", Reference 1], LetFun "g" (Undefined 3))]),
                             ("g", [
                                (Structured [Text "foo ", Reference 1], Value (Text "baz")),
                                (Structured [Text "bar ", Reference 1], Var 1)])]
-}

type VarEnv v = M.Map v Value
type FunEnv m f = M.Map f (WorkspaceId -> Value -> m Value)

-- NOTE: We could do a "parallel" evaluator that might allow multiple workspaces to be scheduled.
evaluateExp :: (Ord f, Ord v, Monad m)
            => (VarEnv v -> WorkspaceId -> f -> Message -> m (VarEnv v, Exp f v))
            -> Exp f v
            -> m Value
evaluateExp match = evaluateExp' match M.empty M.empty

evaluateExp' :: (Ord f, Ord v, Monad m)
             => (VarEnv v -> WorkspaceId -> f -> Message -> m (VarEnv v, Exp f v))
             -> VarEnv v
             -> FunEnv m f
             -> Exp f v
             -> m Value
{-
evaluateExp' :: (VarEnv Var -> WorkspaceId -> Name -> Message -> IO (VarEnv Var, Exp Name Var))
             -> VarEnv Var
             -> FunEnv IO Name
             -> Exp Name Var
             -> IO Value
-}
evaluateExp' match = go
    where go varEnv funEnv (Var x) = return $! case M.lookup x varEnv of Just v -> v
          go varEnv funEnv (Value v) = return v
          go varEnv funEnv (Call ws f e) = do
            v <- go varEnv funEnv e
            (case M.lookup f funEnv of Just f' -> f') ws v
          go varEnv funEnv (LetFun f body) = do
            let fEvaled ws v = do
                    (varEnv', e) <- match varEnv ws f v
                    -- go varEnv' funEnv e -- non-recursive let
                    go varEnv' funEnv' e -- recursive let
                funEnv' = M.insert f fEvaled funEnv
            go varEnv funEnv' body

data Name = ANSWER | LOCAL !Int deriving ( Eq, Ord, Show )

nameToBuilder :: Name -> Builder
nameToBuilder ANSWER = fromText "answer"
nameToBuilder (LOCAL i) = singleton 'f' <> decimal i

{-
type Name = (WorkspaceId, Int)

nameToBuilder :: Name -> Builder
nameToBuilder (f, i) = singleton 'f' <> decimal f <> singleton '_' <> decimal i
-}

type Var = Pointer

type Exp' = Exp Name Var

-- NOTE: Instead of using forkIO and co, we could use a monad other than IO for
-- expression evaluation that supports suspending a computation or implements cooperative
-- concurrency.
makeInterpreterScheduler :: SchedulerContext extra -> IO SchedulerFn
makeInterpreterScheduler ctxt = do
    let answerFn = ANSWER -- (0, 0) :: Name
    alternativesRef <- newIORef (M.empty :: M.Map Name [(Message, Exp')]) -- TODO: Load from database.
    idRef <- newIORef (1 :: Int)

    requestMVar <- newEmptyMVar :: IO (MVar (Maybe WorkspaceId))
    responseMVar <- newEmptyMVar :: IO (MVar Exp')

    let genSym = atomicModifyIORef' idRef (\n -> (n+1, LOCAL n))

        debugCode = do
            altMap <- readIORef alternativesRef
            T.putStrLn (toText (expToBuilder (\f -> maybe [] id $ M.lookup f altMap) (LetFun answerFn (Value (Text "dummy")))))

        blockOnUser mWorkspaceId = do
            putMVar requestMVar mWorkspaceId
            takeMVar responseMVar

        replyFromUser e = do
            putMVar responseMVar e
            takeMVar requestMVar

        match varEnv workspaceId f m = do
            altsMap <- readIORef alternativesRef
            case M.lookup f altsMap of
                Just alts -> do
                    let !mMatch = asum $ map (\(p, e) -> fmap (\bindings -> (M.union varEnv bindings, e)) $ matchMessage p m) alts
                    case mMatch of
                        Just result -> return result
                        Nothing -> do
                            e <- blockOnUser (Just workspaceId)
                            pattern <- generalize ctxt m
                            modifyIORef' alternativesRef (M.insertWith (++) f [(pattern, e)]) -- TODO: Add to end? Technically shouldn't matter.
                            debugCode
                            let !(Just bindings) = matchMessage pattern m -- This shouldn't fail.
                            let varEnv' = M.union varEnv bindings
                            return (varEnv', e)
                Nothing -> do
                    e <- blockOnUser (Just workspaceId)
                    pattern <- generalize ctxt m
                    modifyIORef' alternativesRef (M.insertWith (++) f [(pattern, e)])
                    debugCode
                    let !(Just bindings) = matchMessage pattern m -- This shouldn't fail.
                    let varEnv' = M.union varEnv bindings
                    return (varEnv', e)

        scheduler user workspace (Create msg) = do
            msg <- normalize ctxt msg -- TODO: And generalize?
            newWorkspaceId <- createWorkspace ctxt workspace msg
            let !workspaceId = identity workspace
            f <- genSym
            mNewWorkspaceId <- replyFromUser $ LetFun f (Call workspaceId f (Call newWorkspaceId answerFn (Value msg)))
            case mNewWorkspaceId of
                Just newWorkspaceId -> Just <$> getWorkspace ctxt newWorkspaceId
                Nothing -> return Nothing

        scheduler user workspace (Answer msg) = do
            msg <- normalize ctxt msg
            sendAnswer ctxt workspace msg
            mNewWorkspaceId <- replyFromUser (Value msg)
            case mNewWorkspaceId of
                Just newWorkspaceId -> Just <$> getWorkspace ctxt newWorkspaceId
                Nothing -> return Nothing

        scheduler user workspace (Expand ptr) = do -- TODO: Validate that ptr refers to an in-scope pointer/"variable". Is remapping needed as well?
            expandPointer ctxt workspace ptr
            let !workspaceId = identity workspace
            f <- genSym
            mNewWorkspaceId <- replyFromUser $ LetFun f (Call workspaceId f (Var ptr)) -- TODO: ptr needs to correspond to a "variable"
            case mNewWorkspaceId of
                Just newWorkspaceId -> Just <$> getWorkspace ctxt newWorkspaceId
                Nothing -> return Nothing

        scheduler user workspace (Send ws msg) = do
            -- TODO: Think about this and support it if it makes sense.
            -- sendMessage ctxt workspace ws msg
            putStrLn "makeInterpreterScheduler: Message sending not supported."
            Just <$> getWorkspace ctxt (identity workspace)

    forkIO $ do
        -- let startExp = LetFun answerFn (Call answerFn (Value (Text "What is your question?")))
        e <- takeMVar responseMVar
        case e of
            LetFun _ (Call _ _ e) -> do
                t <- evaluateExp' match M.empty M.empty (LetFun answerFn e)
                T.putStrLn (toText (messageToBuilder t))
                blockOnUser Nothing
                return ()
            _ -> return ()

    return scheduler
