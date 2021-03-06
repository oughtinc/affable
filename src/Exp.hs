{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module Exp ( Result(..), Value, Pattern, Exp(..), Exp', Primitive, Var, Name(..), VarEnv, VarMapping, FunEnv, PrimEnv,
             nameToBuilder, expToBuilder', expToBuilder, expToBuilderDB, expFromDB, expToHaskell,
             evaluateExp, evaluateExp', kont1ToBuilderDB, parseKont1UnsafeDB, nameParser,
             varEnvToBuilder, parseVarEnv, funEnvToBuilder, parseFunEnv,
             GoFn, MatchFn, EvalState, EvalState', Kont1(..), Kont1', Konts(..), Konts', KontsId', applyKonts ) where
import Data.Functor ( (<$) ) -- base
import Data.Int ( Int64 ) -- base
import Data.List ( intersperse ) -- base
import qualified Data.Map as M -- containers
import qualified Data.Text as T -- text
import Data.Void ( Void ) -- base
import Data.Text.Lazy.Builder ( Builder, singleton, fromText ) -- text
import qualified Data.Text.Lazy.Builder.Int as T ( decimal ) -- text
import Text.Megaparsec ( Parsec, parse, (<|>), (<?>), many, sepBy ) -- megaparsec
import Text.Megaparsec.Char ( char, string ) -- megaparsec
import Text.Megaparsec.Char.Lexer ( decimal ) -- megaparsec

import Message ( Message(..), Pointer, messageToBuilder, messageToBuilderDB, messageParser', messageToHaskell, messageToPattern )
import Workspace ( VersionId, versionIdToBuilder, parseVersionId )
import Util ( mapToBuilder, parseMap, parseUnsafe )

type Value = Message
data Result a = Finished Value | Died [a] | Paused deriving ( Show )

-- NOTE: Currently only ever use LetFun f (Call f _) pattern which is essentially, unsurprisingly,
-- equivalent to doing a case analysis on _. It's possible having Call be separate could be useful
-- for tail calls.
data Exp p f v
    = Var v                         -- x
    | Value Value                   -- Structured [Text "foo", Reference 1, Reference 2]
    | Prim p (Exp p f v)            -- p(e)
    | Call f [Exp p f v]            -- f(e1, e2, ..., eN)
    | LetFun f (Exp p f v)          -- let f(Structured [Text "foo", p1, p2]) = e1 -- The cases arise magically, i.e. via lookup indexed by f.
  deriving ( Show )                 --     f(Structured [Text "bar", p1]) = e2
                                    --     f(p1) = e3
                                    -- in e4

type VarEnv v = M.Map v Value
type VarMapping v = M.Map v v
type FunEnv f v = M.Map f (VarEnv v) -- NOTE: Could possibly reduce this to a Var-to-Var mapping if all Values are labeled.
type PrimEnv s m p = M.Map p (s -> Value -> m Value)

type GoFn m s p f v a = VarEnv v -> FunEnv f v -> s -> Exp p f v -> Kont1 s p f v -> m (Result a)
type MatchFn m s p f v a = s -> VarEnv v -> FunEnv f v -> f -> [Value] -> Kont1 s p f v -> m (Result a)

type KontsId s f = (s, f)

data Kont1 s p f v
    = Done
    | PrimKont p s (Kont1 s p f v)
    | NotifyKont !Int !Int (KontsId s f)
  deriving ( Show )

-- TODO: Have sequential or concurrent be a matter of using threads versus sequentially pulling continuations from a queue.
data Konts s p f v = CallKont (FunEnv f v) f s (Kont1 s p f v)
  deriving ( Show )

applyKont1 :: (Monad m, Ord p, Ord f)
           => MatchFn m s p f v a
           -> (KontsId s f -> Int -> Int -> Value -> m (Result a))
           -> PrimEnv s m p
           -> Kont1 s p f v
           -> Value
           -> m (Result a)
applyKont1 match notifyKont primEnv Done v = return (Finished v)
applyKont1 match notifyKont primEnv (PrimKont p s k) v = do
    result <- (case M.lookup p primEnv of Just p' -> p') s v
    applyKont1 match notifyKont primEnv k result
applyKont1 match notifyKont primEnv (NotifyKont argNumber numArgs kId) v = do
    notifyKont kId argNumber numArgs v

applyKonts :: (Monad m, Ord f) => MatchFn m s p f v a -> Konts s p f v -> [Value] -> m (Result a)
applyKonts match (CallKont funEnv f s k) vs = match s (maybe M.empty id $ M.lookup f funEnv) funEnv f vs k

type ExecManyFn m s p f v a = VarEnv v -> FunEnv f v -> s -> [Exp p f v] -> Konts s p f v -> m (Result a)

evaluateExp :: (Ord p, Ord f, Ord v, Monad m)
            => (EvalState s p f v -> m ())
            -> ExecManyFn m s p f v a
            -> MatchFn m s p f v a
            -> (KontsId s f -> Int -> Int -> Value -> m (Result a))
            -> (VarEnv v -> Value -> Value)
            -> PrimEnv s m p
            -> s
            -> Exp p f v
            -> Kont1 s p f v
            -> m (Result a)
evaluateExp record execMany match notifyKont subst primEnv = evaluateExp' record execMany match notifyKont subst primEnv M.empty M.empty

type EvalState s p f v  = (VarEnv v, FunEnv f v, s, Exp p f v, Kont1 s p f v)

evaluateExp' :: (Ord p, Ord f, Ord v, Monad m)
             => (EvalState s p f v -> m ())
             -> ExecManyFn m s p f v a                               -- TODO: Perhaps collect these arguments into a "context" parameter.
             -> MatchFn m s p f v a                                  -- |
             -> (KontsId s f -> Int -> Int -> Value -> m (Result a)) -- |
             -> (VarEnv v -> Value -> Value)                         -- |
             -> PrimEnv s m p                                        -- +
             -> VarEnv v                                             -- + State
             -> FunEnv f v                                           -- |
             -> s                                                    -- |
             -> Exp p f v                                            -- |
             -> Kont1 s p f v                                        -- +
             -> m (Result a)
evaluateExp' record execMany match notifyKont subst primEnv = eval
    where eval varEnv funEnv s (Var x) k = applyKont1 match notifyKont primEnv k $! case M.lookup x varEnv of Just v -> v
          eval varEnv funEnv s (Value v) k = applyKont1 match notifyKont primEnv k $ subst varEnv v
          eval varEnv funEnv s (Prim p e) k = eval' varEnv funEnv s e (PrimKont p s k)
          eval varEnv funEnv s (Call f es) k = execMany varEnv funEnv s es (CallKont funEnv f s k)
          eval varEnv funEnv s (LetFun f body) k = eval' varEnv (M.insert f varEnv funEnv) s body k
          eval' varEnv funEnv s e k = do
            record (varEnv, funEnv, s, e, k)
            return Paused

type Var = Pointer
type Pattern = Message
type Primitive = Int

data Name = ANSWER | LOCAL !Int64 deriving ( Eq, Ord, Show )

type Exp' = Exp Primitive Name Var
type Kont1' = Kont1 VersionId Primitive Name Var
type KontsId' = KontsId VersionId Name
type Konts' = Konts VersionId Primitive Name Var
type EvalState' = EvalState VersionId Primitive Name Var

intersperseChar :: Char -> (a -> Builder) -> [a] -> Builder
intersperseChar c f = mconcat . intersperse (singleton c) . map f

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
          <?> "expression DB"

nameToBuilder :: Name -> Builder
nameToBuilder ANSWER = fromText "answer"
nameToBuilder (LOCAL i) = singleton 'f' <> T.decimal i

expToBuilder' :: (p -> Builder) -> (f -> Builder) -> (v -> Builder) -> (f -> [([Pattern], Exp p f v)]) -> Exp p f v -> Builder
expToBuilder' pBuilder fBuilder vBuilder alternativesFor = go 0
    where go indent (Var x) = vBuilder x
          go indent (Value v) = messageToBuilder v
          go indent (Prim p e) = pBuilder p <> singleton '(' <> go 0 e <> singleton ')'
          go indent (Call f es) = fBuilder f <> singleton '(' <> intersperseChar ',' (go 0) es <> singleton ')'
          go indent (LetFun f body) | null alts = go indent body <> fromText " where "
                                               <> indentBuilder <> fromText "  " <> fBuilder f <> fromText "(_) = undefined"
                                    | otherwise = go indent body <> fromText " where "
                                               <> foldMap (\(ps, e) -> f' ps <> fromText " = " <> go (indent + 2) e) alts
            where !indentBuilder = singleton '\n' <> fromText (T.replicate indent " ")
                  !alts = alternativesFor f
                  f' ps = indentBuilder <> fromText "  " <> fBuilder f <> singleton '(' <> intersperseChar ',' messageToBuilder ps <> singleton ')'

expToBuilder :: (Name -> [([Pattern], Exp')]) -> Exp' -> Builder
expToBuilder = expToBuilder' (\p -> fromText "prim" <> T.decimal p) nameToBuilder (\v -> singleton '$' <> T.decimal v)

expToBuilderDB :: Exp' -> Builder
expToBuilderDB (Var v) = fromText "(Var " <> T.decimal v <> singleton ')'
expToBuilderDB (Value msg) = singleton '[' <> messageToBuilder msg <> singleton ']'
expToBuilderDB (Prim p e) = fromText "(Prim " <> T.decimal p <> singleton ' ' <> expToBuilderDB e <> singleton ')'
expToBuilderDB (Call f es) = fromText "(Call " <> nameToBuilder f <> singleton ' ' <> intersperseChar ' ' expToBuilderDB es <> singleton ')'
expToBuilderDB (LetFun f body) = fromText "(LetFun " <> nameToBuilder f <> singleton ' ' <> expToBuilderDB body <> singleton ')'

expToHaskell :: (Name -> [([Pattern], Exp')]) -> Exp' -> Builder
expToHaskell alternativesFor = go 0
    where go indent (Var x) = messageToHaskell (Reference x)
          go indent (Value v) = messageToHaskell v
          go indent (Prim p e) = fromText "prim" <> T.decimal p <> singleton '(' <> go 0 e <> singleton ')'
          go indent (Call f es) = nameToBuilder f <> singleton '(' <> intersperseChar ',' (go 0) es <> singleton ')'
          go indent (LetFun f body) | null alts = go indent body <> fromText " where "
                                               <> indentBuilder <> fromText "  " <> nameToBuilder f <> fromText "(_) = undefined"
                                    | otherwise = go indent body <> fromText " where "
                                               <> foldMap (\(ps, e) -> f' ps <> fromText " = " <> go (indent + 2) e) alts
            where !indentBuilder = singleton '\n' <> fromText (T.replicate indent " ")
                  !alts = alternativesFor f
                  f' ps = indentBuilder <> fromText "  " <> nameToBuilder f <> singleton '(' <> intersperseChar ',' messageToPattern ps <> singleton ')'

-- TODO: Can fix fBuilder as nameToBuilder as I expect to have f = Name and s = VersionId for keying in the database.
-- NOTE: All this isn't quite as bad as it looks, at least in the concurrent case. If CallKont gets represented by an
-- key for a database table, then Kont1 values will tend to be fairly small.
--
-- The overall idea is that I'll have a database table of Konts keyed by VersionId and Name that holds the contents of
-- CallKonts or maybe JoinKonts. Konts occurring in Kont1 values (which will
-- only be Done, PrimKont, and NotifyKont in the concurrent case), will be represented as VersionId-Name pairs.
-- The actual representation of Kont1 values that are actually used will then be fairly small.
kont1ToBuilderDB' :: (p -> Builder) -> (f -> Builder) -> Kont1 VersionId p f v -> Builder
kont1ToBuilderDB' pBuilder fBuilder = go
    where go Done = fromText "Done"
          go (PrimKont p s k) = fromText "(PrimKont " <> pBuilder p <> singleton ' ' <> versionIdToBuilder s <> singleton ' ' <> go k <> singleton ')'
          go (NotifyKont argNumber numArgs kId) = fromText "(NotifyKont " <> T.decimal argNumber <> singleton ' '
                                                                          <> T.decimal numArgs <> singleton ' '
                                                                          <> kontsIdToBuilderDB fBuilder kId <> singleton ')'

kont1ToBuilderDB :: Kont1' -> Builder
kont1ToBuilderDB = kont1ToBuilderDB' T.decimal nameToBuilder

kont1ParserDB :: Parsec Void T.Text Kont1'
kont1ParserDB = (Done <$ string "Done")
            <|> (PrimKont <$> (string "(PrimKont " *> decimal) <*> (char ' ' *> parseVersionId) <*> (char ' ' *> kont1ParserDB) <* char ')')
            <|> (NotifyKont <$> (string "(NotifyKont " *> decimal) <*> (char ' ' *> decimal) <*> (char ' ' *> kontsIdParserDB) <* char ')')
            <?> "kont1"

parseKont1UnsafeDB :: T.Text -> Kont1'
-- parseKont1UnsafeDB t = case parse kont1ParserDB "" t of Right k -> k
parseKont1UnsafeDB = parseUnsafe kont1ParserDB

kontsIdParserDB :: Parsec Void T.Text KontsId'
kontsIdParserDB = (,) <$> (char '(' *> parseVersionId) <*> (char ',' *> nameParser) <* char ')' <?> "continuation ID"

kontsIdToBuilderDB :: (f -> Builder) -> KontsId VersionId f -> Builder
kontsIdToBuilderDB fBuilder (s, f) = singleton '(' <> versionIdToBuilder s <> singleton ',' <> fBuilder f <> singleton ')'

-- I don't really want to store these as strings or really at all, but, for now, it is the most direct thing to do.
varEnvToBuilder :: VarEnv Var -> Builder
varEnvToBuilder = mapToBuilder T.decimal (\msg -> singleton '[' <> messageToBuilderDB msg <> singleton ']')

parseVarEnv :: Parsec Void T.Text (VarEnv Var)
parseVarEnv = parseMap decimal (char '[' *> messageParser' <* char ']') <?> "variable environment"

funEnvToBuilder :: FunEnv Name Var -> Builder
funEnvToBuilder = mapToBuilder nameToBuilder varEnvToBuilder

parseFunEnv :: Parsec Void T.Text (FunEnv Name Var)
parseFunEnv = parseMap nameParser parseVarEnv <?> "function environment"
