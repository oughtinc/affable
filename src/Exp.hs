{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module Exp ( Value, Pattern, Exp(..), Exp', Primitive, Var, Name(..), VarEnv, VarMapping, FunEnv, PrimEnv,
             nameToBuilder, expToBuilder', expToBuilder, expToBuilderDB, expFromDB, expToHaskell, evaluateExp, evaluateExp' ) where
import Data.Functor ( (<$) ) -- base
import Data.List ( intersperse ) -- base
import qualified Data.Map as M -- containers
import qualified Data.Text as T -- text
import Data.Void ( Void ) -- base
import Data.Text.Lazy.Builder ( Builder, singleton, fromText ) -- text
import qualified Data.Text.Lazy.Builder.Int as T ( decimal ) -- text
import Text.Megaparsec ( Parsec, parse, (<|>), (<?>), many ) -- megaparsec
import Text.Megaparsec.Char ( char, string ) -- megaparsec
import Text.Megaparsec.Char.Lexer ( decimal ) -- megaparsec

import Message ( Message(..), Pointer, messageToBuilder, messageParser', messageToHaskell, messageToPattern )

type Value = Message

-- NOTE: Currently only ever use LetFun f (Call f _) pattern which is essentially, unsurprisingly,
-- equivalent to doing a case analysis on _. It's possible having Call be separate could be useful
-- for "tail calls".
data Exp p f v
    = Var v                         -- x
    | Value Value                   -- Structured [Text "foo", Reference 1, Reference 2]
    | Prim p (Exp p f v)            -- p(e)
    | Call f [Exp p f v]            -- f(e1, e2, ..., eN)
    | LetFun f (Exp p f v)          -- let f(Structured [Text "foo", p1, p2]) = e1 -- The cases arise magically, i.e. via lookup indexed by f.
  deriving ( Read, Show )           --     f(Structured [Text "bar", p1]) = e2
                                    --     f(p1) = e3
                                    -- in e4

type VarEnv v = M.Map v Value
type VarMapping v = M.Map v v
type FunEnv s m f = M.Map f (s -> [Value] -> m Value)
type PrimEnv s m p = M.Map p (s -> Value -> m Value)

-- TODO: Idea for edit: Build dependency graph by executing evaluateExp in a suitable pure monad with
-- a suitable execMany and match. execMany will be something sort of like catMaybes while match
-- will return Nothing on pattern match failure.
evaluateExp :: (Ord p, Ord f, Ord v, Monad m)
            => (s -> [s -> m Value] -> m [Value])
            -> (s -> VarEnv v -> f -> [Value] -> m (s, VarEnv v, Exp p f v))
            -> (VarEnv v -> Value -> Value)
            -> PrimEnv s m p
            -> s
            -> Exp p f v
            -> m Value
evaluateExp execMany match subst primEnv = evaluateExp' execMany match subst primEnv M.empty M.empty

evaluateExp' :: (Ord p, Ord f, Ord v, Monad m)
             => (s -> [s -> m Value] -> m [Value])
             -> (s -> VarEnv v -> f -> [Value] -> m (s, VarEnv v, Exp p f v))
             -> (VarEnv v -> Value -> Value)
             -> PrimEnv s m p
             -> VarEnv v
             -> FunEnv s m f
             -> s
             -> Exp p f v
             -> m Value
evaluateExp' execMany match subst primEnv = go
    where go varEnv funEnv s (Var x) = return $! case M.lookup x varEnv of Just v -> v
          go varEnv funEnv s (Value v) = return $ subst varEnv v
          go varEnv funEnv s (Prim p e) = do
            v <- go varEnv funEnv s e -- NOTE: This is call-by-value. It may be worth experimenting with call-by-name.
            (case M.lookup p primEnv of Just p' -> p') s v
          go varEnv funEnv s (Call f es) = do
            vs <- execMany s $ map (\e s -> go varEnv funEnv s e) es -- NOTE: This is call-by-value. It may be worth experimenting with call-by-name.
            (case M.lookup f funEnv of Just f' -> f') s vs
          go varEnv funEnv s (LetFun f body) = do
            let fEvaled s vs = do
                    (s', varEnv', e) <- match s varEnv f vs
                    -- go varEnv' funEnv s' e -- non-recursive let
                    go varEnv' funEnv' s' e -- recursive let
                funEnv' = M.insert f fEvaled funEnv
            go varEnv funEnv' s body

type Var = Pointer
type Pattern = Message
type Primitive = Int

data Name = ANSWER | LOCAL !Int deriving ( Eq, Ord, Show )

type Exp' = Exp Primitive Name Var

expFromDB :: T.Text -> Exp'
expFromDB t = case parse expParserDB "" t of Right msg -> msg; Left err -> error (show err)

nameParser :: Parsec Void T.Text Name
nameParser = (ANSWER <$ string "answer") <|> (LOCAL <$> (char 'f' *> decimal)) <?> "name"

expParserDB :: Parsec Void T.Text Exp'
expParserDB = (Var <$> (string "(Var " *> decimal <* char ')'))
          <|> (Value <$> (char '[' *> messageParser' <* char ']'))
          <|> (Prim <$> (string "(Prim " *> decimal <* char ' ') <*> (expParserDB <* char ')'))
          <|> (Call <$> (string "(Call " *> nameParser) <*> (many (char ' ' *> expParserDB) <* char ')'))
          <|> (LetFun <$> (string "(LetFun " *> nameParser <* char ' ') <*> (expParserDB <* char ')'))

nameToBuilder :: Name -> Builder
nameToBuilder ANSWER = fromText "answer"
nameToBuilder (LOCAL i) = singleton 'f' <> T.decimal i

expToBuilder' :: (p -> Builder) -> (f -> Builder) -> (v -> Builder) -> (f -> [([Pattern], Exp p f v)]) -> Exp p f v -> Builder
expToBuilder' pBuilder fBuilder vBuilder alternativesFor = go 0
    where go indent (Var x) = vBuilder x
          go indent (Value v) = messageToBuilder v
          go indent (Prim p e) = pBuilder p <> singleton '(' <> go 0 e <> singleton ')'
          go indent (Call f es) = fBuilder f <> singleton '(' <> mconcat (intersperse (singleton ',') (map (go 0) es)) <> singleton ')'
          go indent (LetFun f body) | null alts = go indent body <> fromText " where "
                                               <> indentBuilder <> fromText "  " <> fBuilder f <> fromText "(_) = undefined"
                                    | otherwise = go indent body <> fromText " where "
                                               <> foldMap (\(ps, e) -> f' ps <> fromText " = " <> go (indent + 2) e) alts
            where !indentBuilder = singleton '\n' <> fromText (T.replicate indent " ")
                  !alts = alternativesFor f
                  f' ps = indentBuilder <> fromText "  " <> fBuilder f <> singleton '(' <> mconcat (intersperse (singleton ',') (map messageToBuilder ps)) <> singleton ')'

expToBuilder :: (Name -> [([Pattern], Exp')]) -> Exp' -> Builder
expToBuilder = expToBuilder' (\p -> fromText "prim" <> T.decimal p) nameToBuilder (\v -> singleton '$' <> T.decimal v)

expToBuilderDB :: Exp' -> Builder
expToBuilderDB (Var v) = fromText "(Var " <> T.decimal v <> singleton ')'
expToBuilderDB (Value msg) = singleton '[' <> messageToBuilder msg <> singleton ']'
expToBuilderDB (Prim p e) = fromText "(Prim " <> T.decimal p <> singleton ' ' <> expToBuilderDB e <> singleton ')'
expToBuilderDB (Call f es) = fromText "(Call " <> nameToBuilder f <> singleton ' ' <> mconcat (intersperse (singleton ' ') (map expToBuilderDB es)) <> singleton ')'
expToBuilderDB (LetFun f body) = fromText "(LetFun " <> nameToBuilder f <> singleton ' ' <> expToBuilderDB body <> singleton ')'

expToHaskell :: (Name -> [([Pattern], Exp')]) -> Exp' -> Builder
expToHaskell alternativesFor = go 0
    where go indent (Var x) = messageToHaskell (Reference x)
          go indent (Value v) = messageToHaskell v
          go indent (Prim p e) = fromText "prim" <> T.decimal p <> singleton '(' <> go 0 e <> singleton ')'
          go indent (Call f es) = nameToBuilder f <> singleton '(' <> mconcat (intersperse (singleton ',') (map (go 0) es)) <> singleton ')'
          go indent (LetFun f body) | null alts = go indent body <> fromText " where "
                                               <> indentBuilder <> fromText "  " <> nameToBuilder f <> fromText "(_) = undefined"
                                    | otherwise = go indent body <> fromText " where "
                                               <> foldMap (\(ps, e) -> f' ps <> fromText " = " <> go (indent + 2) e) alts
            where !indentBuilder = singleton '\n' <> fromText (T.replicate indent " ")
                  !alts = alternativesFor f
                  f' ps = indentBuilder <> fromText "  " <> nameToBuilder f <> singleton '(' <> mconcat (intersperse (singleton ',') (map messageToPattern ps)) <> singleton ')'
