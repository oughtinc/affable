module Completions ( CompletionContext(..) ) where
import Exp ( Pattern )
import Scheduler ( SchedulerContext, SessionId )

data CompletionContext extra = CompletionContext {
    completionsFor :: SessionId -> IO [Pattern], -- Or maybe don't have it session specific.
    schedulerContext :: SchedulerContext extra
  }

