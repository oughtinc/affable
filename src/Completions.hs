module Completions ( CompletionContext(..), preparePattern ) where
import qualified Data.Text as T

import Exp ( Pattern )
import Message ( generalizeMessage, stripLabel, parsePatternsUnsafe )
import Scheduler ( SchedulerContext, SessionId )

data CompletionContext extra = CompletionContext {
    completionsFor :: SessionId -> IO [Pattern], -- Or maybe don't have it session specific.
    schedulerContext :: SchedulerContext extra
  }

preparePattern :: T.Text -> Pattern
preparePattern = snd . generalizeMessage 0 . stripLabel . head . parsePatternsUnsafe
