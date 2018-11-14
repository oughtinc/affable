{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Workspace where
import Data.Aeson ( ToJSON, FromJSON ) -- aeson
import Data.Int ( Int64 ) -- base
import qualified Data.Map as M -- containers
import GHC.Generics ( Generic ) -- ghc

import Message
import Time

type Question = Message
type Answer = Message

type WorkspaceId = Int64

-- This will represent the combination of a scratch pad, message history, question/answer.

-- This is what needs to be rendered (possibly with stuff hidden) to the user.
data Workspace = Workspace {
    identity :: WorkspaceId,
    question :: Question,
    subQuestions :: [(Question, Maybe Answer)],
    messageHistory :: [Message], -- TODO: Do we want the history to include who the message was from?
    expandedPointers :: PointerEnvironment,
    time :: Time
    -- ... TODO
  } deriving ( Eq, Show, Generic )

instance FromJSON Workspace
instance ToJSON Workspace

emptyWorkspace :: Message -> Workspace
emptyWorkspace q = Workspace { identity = 0, question = q, subQuestions = [], messageHistory = [], expandedPointers = M.empty, time = 0 }
