{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module Exp ( Value, Pattern, Exp(..), Exp', Var, Name(..),
             nameToBuilder, expToBuilder', expToBuilder, expToBuilderDB, expFromDB, VarEnv, VarMapping, FunEnv, evaluateExp, evaluateExp' ) where
import Data.Functor ( (<$) ) -- base
import qualified Data.Map as M -- containers
import qualified Data.Text as T -- text
import Data.Void ( Void ) -- base
import Data.Text.Lazy.Builder ( Builder, singleton, fromText ) -- text
import qualified Data.Text.Lazy.Builder.Int as T ( decimal ) -- text
import Text.Megaparsec ( Parsec, parse, (<|>), (<?>) ) -- megaparsec
import Text.Megaparsec.Char ( char, string ) -- megaparsec
import Text.Megaparsec.Char.Lexer ( decimal ) -- megaparsec

import Message ( Message, Pointer, messageToBuilder, messageParser' )

type Var = Pointer

type Value = Message
type Pattern = Message

data Name = ANSWER | LOCAL !Int deriving ( Eq, Ord, Show )

nameToBuilder :: Name -> Builder
nameToBuilder ANSWER = fromText "answer"
nameToBuilder (LOCAL i) = singleton 'f' <> T.decimal i

-- TODO: Make data model and figure out how to serialize to database.
-- NOTE: Currently only ever use LetFun f (Call f _) pattern which is essentially, unsurprisingly,
-- equivalent to doing a case analysis on _. It's possible having Call be separate could be useful
-- for "tail calls".
data Exp f v
    = Var v                         -- x
    | Value Value                   -- Structured [Text "foo", Reference 1, Reference 2]
    | Call f (Exp f v)              -- f(e)
    | LetFun f (Exp f v)            -- let f(Structured [Text "foo", p1, p2]) = e1 -- The cases arise magically, i.e. via lookup indexed by f.
  deriving ( Read, Show )           --     f(Structured [Text "bar", p1]) = e2
                                    --     f(p1) = e3
                                    -- in e4

type Exp' = Exp Name Var

expToBuilder' :: (f -> Builder) -> (v -> Builder) -> (f -> [(Pattern, Exp f v)]) -> Exp f v -> Builder
expToBuilder' fBuilder vBuilder alternativesFor = go 0
    where go indent (Var x) = vBuilder x
          go indent (Value v) = valueToBuilder v
          go indent (Call f e) = fBuilder f <> singleton '(' <> go 0 e <> singleton ')'
          {-
          go indent (LetFun f body) | null alts = fromText "let "
                                               <> indentBuilder <> fromText "  " <> fBuilder f <> fromText "(_) = undefined"
                                               <> indentBuilder <> fromText "in " <> go indent body
                                    | otherwise = fromText "let"
                                               <> foldMap (\(p, e) -> f' p <> fromText " = " <> go (indent + 4) e) alts
                                               <> indentBuilder <> fromText "in " <> go indent body
          -}
          go indent (LetFun f body) | null alts = go indent body <> fromText " where "
                                               <> indentBuilder <> fromText "  " <> fBuilder f <> fromText "(_) = undefined"
                                    | otherwise = go indent body <> fromText " where "
                                               <> foldMap (\(p, e) -> f' p <> fromText " = " <> go (indent + 2) e) alts
            where !indentBuilder = singleton '\n' <> fromText (T.replicate indent " ")
                  !alts = alternativesFor f
                  f' p = indentBuilder <> fromText "  " <> fBuilder f <> singleton '(' <> valueToBuilder p <> singleton ')'
          valueToBuilder = messageToBuilder -- TODO: Or use show or something else?

expToBuilder :: (Name -> [(Pattern, Exp Name Var)]) -> Exp Name Var -> Builder
expToBuilder = expToBuilder' nameToBuilder (\v -> singleton '$' <> T.decimal v)

type VarEnv v = M.Map v Value
type VarMapping v = M.Map v v
type FunEnv s m f = M.Map f (s -> Value -> m Value)

-- NOTE: We could do a "parallel" evaluator that might allow multiple workspaces to be scheduled.
evaluateExp :: (Ord f, Ord v, Monad m)
            => (s -> VarEnv v -> f -> Value -> m (s, VarEnv v, Exp f v))
            -> (VarEnv v -> Value -> Value)
            -> s
            -> Exp f v
            -> m Value
evaluateExp match subst = evaluateExp' match subst M.empty M.empty

evaluateExp' :: (Ord f, Ord v, Monad m)
             => (s -> VarEnv v -> f -> Value -> m (s, VarEnv v, Exp f v))
             -> (VarEnv v -> Value -> Value)
             -> VarEnv v
             -> FunEnv s m f
             -> s
             -> Exp f v
             -> m Value
evaluateExp' match subst = go
    where go varEnv funEnv s (Var x) = return $! case M.lookup x varEnv of Just v -> v
          go varEnv funEnv s (Value v) = return $ subst varEnv v
          go varEnv funEnv s (Call f e) = do
            v <- go varEnv funEnv s e -- NOTE: This is call-by-value. It may be worth experimenting with call-by-name.
            (case M.lookup f funEnv of Just f' -> f') s v
          go varEnv funEnv s (LetFun f body) = do
            let fEvaled s v = do
                    (s', varEnv', e) <- match s varEnv f v
                    -- go varEnv' funEnv s' e -- non-recursive let
                    go varEnv' funEnv' s' e -- recursive let
                funEnv' = M.insert f fEvaled funEnv
            go varEnv funEnv' s body

expFromDB :: T.Text -> Exp'
expFromDB t = case parse expParserDB "" t of Right msg -> msg; Left err -> error (show err)

nameParser :: Parsec Void T.Text Name
nameParser = (ANSWER <$ string "answer") <|> (LOCAL <$> (char 'f' *> decimal)) <?> "name"

expParserDB :: Parsec Void T.Text Exp'
expParserDB = (Var <$> (string "(Var " *> decimal <* char ')'))
          <|> (Value <$> (char '[' *> messageParser' <* char ']'))
          <|> (Call <$> (string "(Call " *> nameParser <* char ' ') <*> (expParserDB <* char ')'))
          <|> (LetFun <$> (string "(LetFun " *> nameParser <* char ' ') <*> (expParserDB <* char ')'))

expToBuilderDB :: Exp' -> Builder
expToBuilderDB (Var v) = fromText "(Var " <> T.decimal v <> singleton ')'
expToBuilderDB (Value msg) = singleton '[' <> messageToBuilder msg <> singleton ']'
expToBuilderDB (Call f e) = fromText "(Call " <> nameToBuilder f <> singleton ' ' <> expToBuilderDB e <> singleton ')'
expToBuilderDB (LetFun f body) = fromText "(LetFun " <> nameToBuilder f <> singleton ' ' <> expToBuilderDB body <> singleton ')'
