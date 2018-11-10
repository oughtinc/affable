module Workspace where
import Message
import Time

-- This will represent the combination of a scratch pad, message history, question/answer.
--
-- This is what needs to be rendered (possibly with stuff hidden) to the user. Ideally, automation
-- would 
data Workspace = Workspace {
    question :: Message,
    -- subQuestions :: [QA], -- TODO
    -- expandedPointers :: PointerEnvironment,
    time :: Time
    -- ... TODO

  } deriving ( Show )

-- emptyWorkspace :: Message -> Workspace
-- emptyWorkspace q = Workspace { question = q, subQuestions = [], expandedPointers = M.empty }

