{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Message (
    Message(..), Pointer, Address,
    pointerParser, addressParser, messageParser, parseMessageUnsafe, pointerToBuilder, addressToBuilder, messageToBuilder,
    PointerEnvironment, PointerRemapping, expandPointers, normalizeMessage, generalizeMessage, renumberMessage, renumberAcc )
  where
import Control.Applicative ( (<*>), pure, (*>) ) -- base
import Data.Aeson ( ToJSON, FromJSON ) -- aeson
import Data.Bifunctor ( second ) -- base
import Data.Foldable ( foldl', foldMap ) -- base
import Data.List ( mapAccumL ) -- base
import qualified Data.Map as M -- containers
import qualified Data.Set as S -- containers
import Data.String ( fromString ) -- base
import Data.Text ( Text ) -- text
import Data.Text.Lazy.Builder ( Builder, singleton, fromText ) -- text
import qualified Data.Text.Lazy.Builder.Int as T ( decimal ) -- text
import Data.Void ( Void ) -- base
import GHC.Generics ( Generic ) -- ghc
import Text.Megaparsec ( Parsec, parse, many, some, takeWhile1P, (<|>), (<?>) ) -- megaparsec
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
  deriving ( Eq, Ord, Read, Show, Generic ) -- TODO: Implement custom Show.

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
pointerToBuilder p = singleton '$' <> T.decimal p

addressParser :: Parsec Void Text Address
addressParser =  (char '@' *> decimal) <?> "address"

addressToBuilder :: Address -> Builder
addressToBuilder a = singleton '@' <> T.decimal a

messageParser :: Parsec Void Text Message
messageParser = do
    body <- some mParser <?> "message"
    return $ Structured body
  where mParser = (Reference <$> pointerParser)
              <|> (Location <$> addressParser)
              <|> (Structured <$> (char '[' *> some mParser <* char ']') <?> "submessage")
              <|> (Text <$> takeWhile1P Nothing (\c -> c `notElem` ("[]$@" :: String)) <?> "text")

parseMessageUnsafe :: Text -> Message
parseMessageUnsafe t = case parse messageParser "" t of Right msg -> msg

messageToBuilder :: Message -> Builder
messageToBuilder = go True
    where go  True (Structured ms) = foldMap (go False) ms
          go False (Structured ms) = singleton '[' <> foldMap (go False) ms <> singleton ']'
          go     _ (Text t) = fromText t
          go     _ (Reference p) = pointerToBuilder p
          go     _ (Location a) = addressToBuilder a

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
    where go True env (Structured ms) = second Structured $ mapAccumL (go False) env ms
          go _ env (Structured ms)
            = let p = M.size env + start
                  env' = M.insert p (Structured ms') env
                  (env'', ms') = mapAccumL (go False) env' ms -- A bit of knot typing occurring here.
              in (env'', Reference p)
          go _ env m = (env, m)

-- Creates a message where all pointers are distinct. The output is a mapping
-- from fresh pointers to the old pointers.
generalizeMessage :: Int -> Message -> (PointerRemapping, Message)
generalizeMessage fresh msg = case go (S.empty, M.empty, fresh) msg of ((_, mapping, _), m) -> (mapping, m)
    where go s (Structured ms) = second Structured $ mapAccumL go s ms
          go (seen, mapping, fresh) m@(Reference p)
            | p `S.member` seen = ((seen, M.insert fresh p mapping, fresh+1), Reference fresh)
            | otherwise = ((S.insert p seen, mapping, fresh), m)
          go s m = (s, m)

-- Partial if the PointerRemapping doesn't include every pointer in the Message.
renumberMessage :: PointerRemapping -> Message -> Message
renumberMessage mapping (Structured ms) = Structured $ map (renumberMessage mapping) ms
renumberMessage mapping (Reference p) = case M.lookup p mapping of Just p' -> Reference p'
renumberMessage mapping msg = msg

renumberAcc :: PointerRemapping -> Message -> PointerRemapping
renumberAcc mapping (Structured ms) = foldl' renumberAcc mapping ms
renumberAcc mapping (Reference p) = if p `M.member` mapping then mapping else M.insert p (M.size mapping) mapping
renumberAcc mapping msg = mapping
