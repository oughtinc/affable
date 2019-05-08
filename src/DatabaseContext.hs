module DatabaseContext ( DatabaseContext(..), Snapshot(..) ) where
import qualified Data.Map as M -- containers

import AutoScheduler (  AutoSchedulerContext, FunctionId, ProcessId )
import Completions ( CompletionContext )
import Exp  ( Exp', Pattern, Konts', EvalState' )
import Message ( Message, Pointer, PointerRemapping )
import Scheduler ( SchedulerContext, SessionId )
import Workspace ( Workspace, VersionId )

data Snapshot = Snapshot {
    functionCounterS :: FunctionId,
    workspacesS :: M.Map VersionId Workspace,
    -- messagesS :: ,
    answersS :: M.Map VersionId Message,
    answerFunctionsS :: M.Map SessionId FunctionId,
    pointersS :: M.Map Pointer Message,
    alternativesS :: M.Map FunctionId [([Pattern], Exp')],
    linksS :: M.Map VersionId PointerRemapping,
    continuationsS :: M.Map VersionId (M.Map FunctionId Konts'),
    continuationArgumentsS :: M.Map (VersionId, FunctionId) (M.Map Int Message),
    traceS :: [(ProcessId, EvalState')], -- Stored newest first.
    runQueueS :: M.Map SessionId (M.Map ProcessId EvalState'),
    sessionsS :: M.Map SessionId [ProcessId] }
  deriving ( Show )

data DatabaseContext e = DatabaseContext {
    initDB :: IO (),
    closeDB :: IO (),
    primitivesToHaskell :: IO (),
    snapshot :: IO Snapshot,
    makeSchedulerContext :: IO (SchedulerContext e),
    makeAutoSchedulerContext :: SchedulerContext e -> SessionId -> IO (AutoSchedulerContext e),
    makeCompletionContext :: SchedulerContext e -> IO (CompletionContext e)
  }
