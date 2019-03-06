module DatabaseContext ( DatabaseContext(..) ) where
import AutoScheduler (  AutoSchedulerContext )
import Completions ( CompletionContext )
import Scheduler ( SchedulerContext, SessionId )

data DatabaseContext e = DatabaseContext {
    initDB :: IO (),
    closeDB :: IO (),
    primitivesToHaskell :: IO (),
    makeSchedulerContext :: IO (SchedulerContext e),
    makeAutoSchedulerContext :: SchedulerContext e -> SessionId -> IO (AutoSchedulerContext e),
    makeCompletionContext :: SchedulerContext e -> IO (CompletionContext e)
  }
