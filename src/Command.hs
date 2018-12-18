{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Command( Command(..), commandParser, commandToBuilder ) where
import Control.Applicative ( (<*>), pure, (*>) ) -- base
import Data.Aeson ( ToJSON, FromJSON ) -- aeson
import qualified Data.Map as M -- containers
import Data.Text ( Text ) -- text
import Data.Text.Lazy.Builder ( Builder, fromText ) -- text
import Data.Void ( Void ) -- base
import GHC.Generics ( Generic ) -- ghc
import Text.Megaparsec ( Parsec, (<|>), (<?>) ) -- megaparsec
import Text.Megaparsec.Char ( string', space1 ) -- megaparsec

import Message ( Message, Pointer, Address, messageParser, pointerParser, addressParser, messageToBuilder, addressToBuilder, pointerToBuilder )

data Command
    = Ask Message
    | Reply Message
    | View Pointer
    | Send Address Message
    | Wait
  deriving ( Eq, Ord, Show, Generic ) -- TODO: Implement custom Show.

instance FromJSON Command
instance ToJSON Command

{-
-- Case-insensitive strings.
Cmd ::= "ask" WS Msg
      | "reply" WS Msg
      | "view" WS Pointer
      | "send" WS Address WS Msg
      | "wait"
-}

commandParser :: Parsec Void Text Command
commandParser = (Ask <$> (string' "ask" *> space1 *> messageParser))
            <|> (Reply <$> (string' "reply" *> space1 *> messageParser))
            <|> (View <$> (string' "view" *> space1 *> pointerParser))
            <|> (Send <$> (string' "send" *> space1 *> addressParser) <*> (space1 *> messageParser))
            <|> (Wait <$ string' "wait") <?> "command"

commandToBuilder :: Command -> Builder
commandToBuilder (Ask msg) = fromText "ask " <> messageToBuilder msg
commandToBuilder (Reply msg) = fromText "reply " <> messageToBuilder msg
commandToBuilder (Send a msg) = fromText "send " <> addressToBuilder a <> messageToBuilder msg
commandToBuilder (View p) = fromText "view " <> pointerToBuilder p
commandToBuilder Wait = fromText "wait"
