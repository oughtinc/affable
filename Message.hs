{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Message (
    Message(..), Pointer, Address, PointerEnvironment, PointerRemapping,
    pointerParser, addressParser, messageParser, messageParser', parseMessageUnsafe, parseMessageUnsafe',
    pointerToBuilder, addressToBuilder, messageToBuilder, messageToBuilderDB,
    expandPointers, substitute, normalizeMessage, generalizeMessage, renumberMessage', renumberMessage, renumberAcc,
    instantiatePattern, matchMessage, matchPointers, collectPointers )
  where
import Control.Applicative ( (<*>), pure, (*>) ) -- base
import Data.Aeson ( ToJSON, FromJSON ) -- aeson
import Data.Bifunctor ( second ) -- base
import Data.Foldable ( foldl', foldMap ) -- base
import Data.List ( mapAccumL ) -- base
import qualified Data.Map as M -- containers
import qualified Data.Set as S -- containers
import Data.Traversable ( sequenceA, traverse ) -- base
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
    = Text !Text
    | Reference !Pointer
    | Location !Address
    | Structured [Message]
    | LabeledStructured !Pointer [Message]
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
      | "[" Pointer "|" Msg* "]"
-}

pointerParser :: Parsec Void Text Pointer
pointerParser =  (char '$' *> decimal) <?> "pointer"

pointerToBuilder :: Pointer -> Builder
pointerToBuilder p = singleton '$' <> T.decimal p

addressParser :: Parsec Void Text Address
addressParser =  (char '@' *> decimal) <?> "address"

addressToBuilder :: Address -> Builder
addressToBuilder a = singleton '@' <> T.decimal a

-- Doesn't parse LabeledStructures, so we can keep users for entering them.
messageParser :: Parsec Void Text Message
messageParser = do
    body <- some mParser <?> "message"
    return $ Structured body
  where mParser = (Reference <$> pointerParser)
              <|> (Location <$> addressParser)
              <|> (Structured <$> (char '[' *> some mParser <* char ']') <?> "submessage")
              <|> (Text <$> takeWhile1P Nothing (\c -> c `notElem` ("[]$@" :: String)) <?> "text")

messageParser' :: Parsec Void Text Message
messageParser' = do
    body <- some mParser <?> "message"
    return $ Structured body
  where mParser = (Reference <$> pointerParser)
              <|> (Location <$> addressParser)
              <|> ((char '[' *> (mParser >>= structuredTail)) <?> "submessage")
              <|> (Text <$> takeWhile1P Nothing (\c -> c `notElem` ("[]$@" :: String)) <?> "text")
        structuredTail m@(Reference p) = (LabeledStructured p <$> (char '|' *> some mParser <* char ']'))
                                     <|> (Structured . (m:) <$> many mParser <* char ']')
        structuredTail m = (Structured . (m:) <$> many mParser) <* char ']'

parseMessageUnsafe :: Text -> Message
parseMessageUnsafe t = case parse messageParser' "" t of Right msg -> msg

parseMessageUnsafe' :: Pointer -> Text -> Message
parseMessageUnsafe' p t = case parse messageParser' "" t of Right (Structured ms) -> LabeledStructured p ms

messageToBuilder :: Message -> Builder
messageToBuilder = go True
    where go  True (Structured ms) = foldMap (go False) ms
          go False (Structured ms) = singleton '[' <> foldMap (go False) ms <> singleton ']'
          go     _ (LabeledStructured p ms) = singleton '[' <> pointerToBuilder p <> singleton '|' <> foldMap (go False) ms <> singleton ']'
          go     _ (Text t) = fromText t
          go     _ (Reference p) = pointerToBuilder p
          go     _ (Location a) = addressToBuilder a

messageToBuilderDB :: Message -> Builder
messageToBuilderDB = go True
    where go  True (Structured ms) = foldMap (go False) ms
          go  True (LabeledStructured p ms) = foldMap (go False) ms
          go False (Structured ms) = singleton '[' <> foldMap (go False) ms <> singleton ']'
          go     _ (LabeledStructured p ms) = singleton '[' <> pointerToBuilder p <> singleton '|' <> foldMap (go False) ms <> singleton ']'
          go     _ (Text t) = fromText t
          go     _ (Reference p) = pointerToBuilder p
          go     _ (Location a) = addressToBuilder a

-- TODO: Change this.
type PointerEnvironment = M.Map Pointer Message

-- TODO: Use strict map.
type PointerRemapping = M.Map Pointer Pointer

-- Expand the pointers in the pointer environment that occur in the message. See substitute.
-- The idea is that a Message with pointers or with those pointers expanded are "equivalent".
-- We want to maintain the invariant that for LabeledStructure p msg, msg is "equivalent" to
-- whatever "p" points at (in the actual database).
expandPointers :: PointerEnvironment -> Message -> Message
expandPointers env (Reference p) = case M.lookup p env of
                                        Nothing -> Reference p
                                        Just m -> expandPointers env m
expandPointers env (Structured blocks) = Structured (map (expandPointers env) blocks)
expandPointers env (LabeledStructured p blocks) = LabeledStructured p (map (expandPointers env) blocks)
expandPointers env t = t

-- substitute is like expandPointers only the PointerEnvironment may not reflect the actual state of the
-- pointers in the database. The upshot of this is that when we substitute under a LabeledStructure p, the
-- result may no longer be "equivalent" to what the pointer p points to, so, to maintain the invariant
-- we turn those into plain Structures.
substitute :: PointerEnvironment -> Message -> Message
substitute env (Reference p) = case M.lookup p env of
                                    Nothing -> Reference p
                                    Just m -> substitute env m
substitute env (Structured [p@(Reference _)]) = substitute env p
substitute env (Structured blocks) = Structured (map (substitute env) blocks)
substitute env (LabeledStructured p blocks) = Structured (map (substitute env) blocks) -- TODO: Could probably keep LabeledStructured p if we don't
                                                                                       -- actually substitute into any subMessages. We could percolate
                                                                                       -- up a Bool to indicate whether any substitutions were made.
substitute _ t = t

-- Given a Message, replace all Structured sub-Messages with pointers and output a mapping
-- from those pointers to the Structured sub-Messages. Normalized subMessages are "equivalent"
-- so the labels don't need to change on LabeledStructures.
normalizeMessage :: Int -> Message -> (PointerEnvironment, Message)
normalizeMessage start = go True M.empty
    where go True env (Structured ms) = (env'', LabeledStructured p ms')
            where !p = M.size env + start
                  env' = M.insert p (LabeledStructured p ms') env -- Knot tying
                  (env'', ms') = mapAccumL (go False) env' ms
          -- go True env (Structured ms) = second Structured $ mapAccumL (go False) env ms -- TODO: Make this labeled too? XXX
          go True env (LabeledStructured p ms) = second (LabeledStructured p) $ mapAccumL (go False) env ms
          go _ env (Structured ms) = (env'', Reference p)
            where !p = M.size env + start
                  env' = M.insert p (LabeledStructured p ms') env
                  (env'', ms') = mapAccumL (go False) env' ms -- A bit of knot typing occurring here.
          go _ env m@(LabeledStructured p ms) = (env, Reference p)
          {-
          go _ env (LabeledStructured p ms) -- TODO: Or do I want to just leave this after processing the body?
            = let env' = M.insert p (LabeledStructured p ms') env
                  (env'', ms') = mapAccumL (go False) env' ms -- A bit of knot typing occurring here.
              in (env'', Reference p)
          -}
          go _ env m = (env, m)

-- Creates a message where all pointers are distinct. The output is a mapping
-- from fresh pointers to the old pointers.
{-
-- Replaces all pointers with distinct pointers.
generalizeMessage :: Int -> Message -> (PointerRemapping, Message)
generalizeMessage fresh msg = go M.empty msg
    where go !mapping (Structured ms) = second Structured $ mapAccumL go mapping ms
          go !mapping m@(LabeledStructured old ms) = second (LabeledStructured new) $ mapAccumL go (M.insert new old mapping) ms
            where !new = M.size mapping + fresh
          go !mapping m@(Reference old) = (M.insert new old mapping, Reference new)
            where !new = M.size mapping + fresh
          go !s m = (s, m)
-}
-- Only makes pointers for duplicates. Reuses the old pointers.
generalizeMessage :: Int -> Message -> (PointerRemapping, Message)
generalizeMessage fresh msg = case go (S.empty, M.empty, fresh) msg of ((_, mapping, _), m) -> (mapping, m)
    where go s (Structured ms) = second Structured $ mapAccumL go s ms
          go (!seen, !mapping, !fresh) (LabeledStructured p ms)
            | p `S.member` seen = second (LabeledStructured fresh) $ mapAccumL go (seen, M.insert fresh p mapping, fresh+1) ms
            | otherwise = second (LabeledStructured p) $ mapAccumL go (S.insert p seen, mapping, fresh) ms
          go (!seen, !mapping, !fresh) m@(Reference p)
            | p `S.member` seen = ((seen, M.insert fresh p mapping, fresh+1), Reference fresh)
            | otherwise = ((S.insert p seen, mapping, fresh), m)
          go s m = (s, m)

-- Replace pointers in Message with new pointers that point to the Messages in the PointerEnvironment.
instantiatePattern :: Int -> PointerEnvironment -> Message -> (PointerEnvironment, PointerRemapping, Message)
instantiatePattern fresh env msg = case go (M.empty, M.empty) msg of ((env', mapping), msg) -> (env', mapping, msg)
    where go s (Structured ms) = second Structured $ mapAccumL go s ms
          -- go s (LabeledStructured p ms) = second (LabeledStructured p) $ mapAccumL go s ms -- TODO: Keep this label if the contents no longer match?
          go s (LabeledStructured p ms) = second Structured $ mapAccumL go s ms
          go (env', mapping) (Reference old) = case M.lookup old env of
                                                    Just m@(Reference new) -> ((env', M.insert new old mapping), m)
                                                    Just m -> let !new = M.size mapping + fresh
                                                              in ((M.insert new m env', M.insert new old mapping), Reference new)
          go s m = (s, m)

renumberMessage' :: PointerRemapping -> Message -> Message
renumberMessage' mapping (Structured ms) = Structured $ map (renumberMessage' mapping) ms
renumberMessage' mapping (LabeledStructured p ms) = LabeledStructured (maybe p id $ M.lookup p mapping) (map (renumberMessage' mapping) ms)
renumberMessage' mapping (Reference p) = Reference $ maybe p id $ M.lookup p mapping
renumberMessage' mapping msg = msg

-- Partial if the PointerRemapping doesn't include every pointer in the Message.
-- This violates the invariant with respect to LabeledStructures, but should maintain it modulo the PointerRemapping.
-- For what renumberMessage is used for, this is okay.
renumberMessage :: PointerRemapping -> Message -> Maybe Message
renumberMessage mapping (Structured ms) = Structured <$> traverse (renumberMessage mapping) ms
renumberMessage mapping (LabeledStructured p ms) = LabeledStructured <$> M.lookup p mapping <*> traverse (renumberMessage mapping) ms
renumberMessage mapping (Reference p) = Reference <$> M.lookup p mapping
renumberMessage mapping msg = Just msg

renumberAcc :: PointerRemapping -> Message -> PointerRemapping
renumberAcc mapping (Structured ms) = foldl' renumberAcc mapping ms
renumberAcc mapping (LabeledStructured p ms) = foldl' renumberAcc mapping' ms
    where !mapping' = if p `M.member` mapping then mapping else M.insert p (M.size mapping) mapping
renumberAcc mapping (Reference p) = if p `M.member` mapping then mapping else M.insert p (M.size mapping) mapping
renumberAcc mapping msg = mapping

-- This mostly assumes that the 'pattern' fits the Message.
matchPointers :: Message -> Message -> PointerRemapping
matchPointers (Structured pms) (Structured ms) = M.unions $ zipWith matchPointers pms ms
matchPointers (Structured pms) (LabeledStructured _ ms) = M.unions $ zipWith matchPointers pms ms
matchPointers (LabeledStructured p pms) (Structured ms) = M.unions $ zipWith matchPointers pms ms
matchPointers (LabeledStructured p pms) (LabeledStructured l ms) = (M.insert l p . M.unions) $ zipWith matchPointers pms ms
matchPointers (Reference p) (Reference l) = M.singleton l p
matchPointers (Reference p) (LabeledStructured l ms) = M.singleton l p
matchPointers _ _ = M.empty

-- This assumes `pattern` has no duplicated pointers.
matchMessage :: Message -> Message -> Maybe PointerEnvironment
matchMessage (Text pt) (Text t) | pt == t = Just M.empty
matchMessage (Location pa) (Location a) | pa == a = Just M.empty
matchMessage (Structured pms) (Structured ms) = M.unions <$> sequenceA (zipWith matchMessage pms ms)
matchMessage (Structured pms) (LabeledStructured _ ms) = M.unions <$> sequenceA (zipWith matchMessage pms ms)
-- matchMessage (LabeledStructured p pms) (Structured ms)
--     = (M.insert p (LabeledStructured p ms) . M.unions) <$> sequenceA (zipWith matchMessage pms ms)
matchMessage (LabeledStructured p pms) m@(Structured ms)
    = (M.insert p m . M.unions) <$> sequenceA (zipWith matchMessage pms ms)
-- matchMessage (LabeledStructured p pms) m@(LabeledStructured _ ms)
--     = (M.insert p m . M.unions) <$> sequenceA (zipWith matchMessage pms ms)
matchMessage (LabeledStructured p pms) (LabeledStructured _ ms)
    = (M.insert p (Structured ms) . M.unions) <$> sequenceA (zipWith matchMessage pms ms)
matchMessage (Reference p) m@(Reference _) = Just $ M.singleton p m
-- matchMessage (Reference p) (Structured ms) = Just $ M.singleton p (LabeledStructured p ms)
matchMessage (Reference p) (Structured ms) = Just $ M.singleton p (Structured ms)
-- matchMessage (Reference p) (LabeledStructured _ ms) = Just $ M.singleton p (LabeledStructured p ms)
matchMessage (Reference p) (LabeledStructured _ ms) = Just $ M.singleton p (Structured ms)
matchMessage _ _ = Nothing
{-
matchMessage = go True
    where go True (LabeledStructured p pms) m@(LabeledStructured l ms)
            = (M.insert p (Reference l) . M.unions) <$> sequenceA (zipWith (go False) pms ms)
          go _ (Text pt) (Text t) | pt == t = Just M.empty
          go _ (Location pa) (Location a) | pa == a = Just M.empty
          go _ (Structured pms) (Structured ms) = M.unions <$> sequenceA (zipWith (go False) pms ms)
          go _ (LabeledStructured p pms) m@(Structured ms)
            = (M.insert p m . M.unions) <$> sequenceA (zipWith (go False) pms ms)
          go _ (LabeledStructured p pms) (LabeledStructured _ ms)
            = (M.insert p (Structured ms) . M.unions) <$> sequenceA (zipWith (go False) pms ms)
          go _ (Reference p) m@(Reference _) = Just $ M.singleton p m
          go _ (Reference p) (Structured ms) = Just $ M.singleton p (Structured ms)
          go _ (Reference p) (LabeledStructured _ ms) = Just $ M.singleton p (Structured ms)
          go _ _ _ = Nothing
-}

collectPointers :: Message -> [Pointer]
collectPointers msg = go msg []
    where go (Reference p) acc = p:acc
          go (Structured ms) acc = foldr go acc ms
          go (LabeledStructured _ ms) acc = foldr go acc ms -- TODO: Count the pointer in the LabeledStructure?
          go _ acc = acc
