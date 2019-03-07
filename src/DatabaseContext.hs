module DatabaseContext ( DatabaseContext(..), Snapshot(..) ) where
import qualified Data.Map as M -- containers

import AutoScheduler (  AutoSchedulerContext, FunctionId, ProcessId )
import Completions ( CompletionContext )
import Exp  ( Exp', Pattern, Konts', EvalState' )
import Message ( Message, Pointer, PointerRemapping )
import Scheduler ( SchedulerContext, SessionId )
import Workspace ( Workspace, WorkspaceId )

data Snapshot = Snapshot {
    workspacesS :: M.Map WorkspaceId Workspace,
    -- messagesS :: ,
    answersS :: M.Map WorkspaceId Message,
    answerFunctionsS :: M.Map SessionId FunctionId,
    pointersS :: M.Map Pointer Message,
    alternativesS :: M.Map FunctionId [([Pattern], Exp')],
    linksS :: M.Map WorkspaceId PointerRemapping,
    continuationsS :: M.Map WorkspaceId (M.Map FunctionId Konts'),
    continuationArgumentsS :: M.Map (WorkspaceId, FunctionId) (M.Map Int Message),
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
