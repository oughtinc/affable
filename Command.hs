{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Command where
import Control.Applicative ( (<*>), pure, (*>) ) -- base
import Data.Aeson ( ToJSON, FromJSON ) -- aeson
import qualified Data.Map as M -- containers
import Data.Text ( Text ) -- text
import Data.Void ( Void ) -- base
import GHC.Generics ( Generic ) -- ghc
import Text.Megaparsec ( Parsec, (<|>), (<?>) ) -- megaparsec
import Text.Megaparsec.Char ( string', space1 ) -- megaparsec

import Message

data Command 
    = Ask Message
    | Reply Message
    | View Pointer
    | Send Address Message
  deriving ( Eq, Ord, Show, Generic ) -- TODO: Implement custom Show.

instance FromJSON Command
instance ToJSON Command

{-
-- Case-insensitive strings.
Cmd ::= "ask" WS Msg
      | "reply" WS Msg
      | "view" WS Pointer
      | "send" WS Address WS Msg
-}

commandParser :: Parsec Void Text Command
commandParser = (Ask <$> (string' "ask" *> space1 *> messageParser))
            <|> (Reply <$> (string' "reply" *> space1 *> messageParser))
            <|> (View <$> (string' "view" *> space1 *> pointerParser))
            <|> (Send <$> (string' "send" *> space1 *> addressParser) <*> (space1 *> messageParser)) <?> "command"
