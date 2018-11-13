{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Message (
    Message(..), Pointer, Address,
    pointerParser, addressParser, messageParser, pointerToBuilder, addressToBuilder, messageToBuilder,
    PointerEnvironment, PointerRemapping, expandPointers, normalizeMessage, renumberMessage )
  where
import Control.Applicative ( (<*>), pure, (*>) ) -- base
import Data.Aeson ( ToJSON, FromJSON ) -- aeson
import Data.Foldable ( foldMap ) -- base
import Data.List ( mapAccumL ) -- base
import qualified Data.Map as M -- containers
import Data.String ( fromString ) -- base
import Data.Text ( Text ) -- text
import Data.Text.Lazy.Builder ( Builder, singleton, fromText ) -- text
import Data.Void ( Void ) -- base
import GHC.Generics ( Generic ) -- ghc
import Text.Megaparsec ( Parsec, many, some, takeWhile1P, (<|>), (<?>) ) -- megaparsec
import Text.Megaparsec.Char ( char ) -- megaparsec
import Text.Megaparsec.Char.Lexer ( decimal ) -- megaparsec

type Pointer = Int
type Address = Int

-- TODO: Perhaps add support for logic variables (?X perhaps) so this code can be shared.
data Message
    = Text Text
    | Reference Pointer
    | Location Address
    | Structured [Message]
  deriving ( Eq, Ord, Show, Generic ) -- TODO: Implement custom Show.

instance FromJSON Message
instance ToJSON Message

{-
Pointer ::= "$" [1-9]*[0-9]

Address ::= "@" [1-9]*[0-9]

Msg ::= Pointer
      | Address
      | [^\]\[]+
      | "[" Msg* "]"
-}

pointerParser :: Parsec Void Text Pointer
pointerParser =  (char '$' *> decimal) <?> "pointer"

pointerToBuilder :: Pointer -> Builder
pointerToBuilder p = singleton '$' <> fromString (show p)

addressParser :: Parsec Void Text Address
addressParser =  (char '@' *> decimal) <?> "address"

addressToBuilder :: Address -> Builder
addressToBuilder a = singleton '@' <> fromString (show a)

messageParser :: Parsec Void Text Message
messageParser = Structured <$> some mParser <?> "message"
    where mParser = (Reference <$> pointerParser)
                <|> (Location <$> addressParser)
                <|> (Structured <$> (char '[' *> many mParser <* char ']') <?> "submessage")
                <|> (Text <$> takeWhile1P Nothing (\c -> c `notElem` ("[]$@" :: String)) <?> "text")

messageToBuilder :: Message -> Builder
messageToBuilder (Text t) = fromText t
messageToBuilder (Reference p) = pointerToBuilder p
messageToBuilder (Location a) = addressToBuilder a
messageToBuilder (Structured ms) = singleton '[' <> foldMap messageToBuilder ms <> singleton ']'

-- TODO: Change this.
type PointerEnvironment = M.Map Pointer Message

-- TODO: Use strict map.
type PointerRemapping = M.Map Pointer Pointer

-- Expand the pointers in the pointer environment that occur in the message.
expandPointers :: PointerEnvironment -> Message -> Message
expandPointers env       (Reference p) = case M.lookup p env of
                                            Nothing -> Reference p
                                            Just m -> expandPointers env m
expandPointers env (Structured blocks) = Structured (map (expandPointers env) blocks)
expandPointers env                   t = t

-- Given a Message, replace all Structured sub-Messages with pointers and output a mapping
-- from those pointers to the Structured sub-Messages.
normalizeMessage :: Int -> Message -> (PointerEnvironment, Message)
normalizeMessage start = go True M.empty
    where go isTopLevel env m@(Structured ms)
            = let p = M.size env + start
                  result = if isTopLevel then Structured ms' else Reference p
                  env' = M.insert p (Structured ms') env
                  (env'', ms') = mapAccumL (go False) env' ms -- A bit of knot typing occurring here.
              in (env'', result)
          go _ env m = (env, m)

-- Renumber the pointers so that they are labelled from `start`. Producing a mapping from
-- the new (local) pointers to the old (global) pointers.
renumberMessage :: Int -> Message -> (PointerRemapping, Message)
renumberMessage start m = let (env, m') = go M.empty m in (invertMap env, m')
    where go env (Reference p) = case M.lookup p env of
                                    Just n -> (env, Reference n)
                                    Nothing -> let !n = M.size env + start
                                               in (M.insert p n env, Reference n)
          go env (Structured ms) = let (env', ms') = mapAccumL go env ms
                                   in (env', Structured ms')
          go env m = (env, m)

          invertMap = M.fromList . map (\(x,y) -> (y,x)) . M.assocs
