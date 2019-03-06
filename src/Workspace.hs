{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Workspace ( Workspace(..), Question, Answer, WorkspaceId,
                   workspaceIdFromText, workspaceIdToBuilder, parseWorkspaceId, newWorkspaceId ) where
import Data.Aeson ( ToJSON, FromJSON ) -- aeson
import Data.Text.Lazy.Builder ( Builder ) -- text
import qualified Data.Text as T -- text
import Data.UUID ( UUID ) -- uuid
import qualified Data.UUID.V4 as UUID ( nextRandom ) -- uuid
import Data.Void ( Void ) -- base
import qualified Data.Map as M -- containers
import GHC.Generics ( Generic ) -- ghc
import Text.Megaparsec ( Parsec, parseMaybe ) -- megaparsec

import Message ( Message, PointerEnvironment )
import Time ( Time )
import Util ( uuidToBuilder, parseUUID )

type Question = Message
type Answer = Message

type WorkspaceId = UUID

-- This will represent the combination of a scratch pad, message history, question/answer.

-- This is what needs to be rendered (possibly with stuff hidden) to the user.
data Workspace = Workspace {
    identity :: !WorkspaceId,
    parentId :: Maybe WorkspaceId,
    question :: Question,
    subQuestions :: [(WorkspaceId, Question, Maybe Answer)],
    messageHistory :: [Message], -- TODO: Do we want the history to include who the message was from?
    expandedPointers :: PointerEnvironment,
    time :: Time
  } deriving ( Eq, Show, Generic )

instance FromJSON Workspace
instance ToJSON Workspace

workspaceIdFromText :: T.Text -> WorkspaceId
workspaceIdFromText t = case parseMaybe parseWorkspaceId t of Just wsId -> wsId

workspaceIdToBuilder :: WorkspaceId -> Builder
workspaceIdToBuilder = uuidToBuilder

parseWorkspaceId :: Parsec Void T.Text WorkspaceId
parseWorkspaceId = parseUUID

newWorkspaceId :: IO WorkspaceId
newWorkspaceId = UUID.nextRandom
