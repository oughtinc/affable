{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Workspace where
import Data.Aeson ( ToJSON, FromJSON ) -- aeson
import GHC.Generics ( Generic ) -- ghc

import Message
import Time

-- This will represent the combination of a scratch pad, message history, question/answer.
--
-- This is what needs to be rendered (possibly with stuff hidden) to the user.
data Workspace = Workspace {
    question :: Message,
    -- subQuestions :: [QA], -- TODO
    -- expandedPointers :: PointerEnvironment,
    time :: Time
    -- ... TODO

  } deriving ( Eq, Show, Generic )

instance FromJSON Workspace
instance ToJSON Workspace

-- emptyWorkspace :: Message -> Workspace
-- emptyWorkspace q = Workspace { question = q, subQuestions = [], expandedPointers = M.empty }
