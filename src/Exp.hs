{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module Exp ( Value, Pattern, Exp(..), Exp', Primitive, Var, Name(..), VarEnv, VarMapping, FunEnv, PrimEnv,
             nameToBuilder, expToBuilder', expToBuilder, expToBuilderDB, expFromDB, expToHaskell,
             evaluateExp, evaluateExp', sequenceK, concurrentlyK, kont1ToBuilderDB,
             GoFn, MatchFn, Kont1(..), Kont1', Konts(..), Konts', applyKont1, applyKonts ) where
import Data.Functor ( (<$) ) -- base
import Data.List ( intersperse ) -- base
import qualified Data.Map as M -- containers
import qualified Data.Text as T -- text
import Data.Void ( Void ) -- base
import Data.Text.Lazy.Builder ( Builder, singleton, fromText ) -- text
import qualified Data.Text.Lazy.Builder.Int as T ( decimal ) -- text
import Text.Megaparsec ( Parsec, parse, (<|>), (<?>), many, sepBy ) -- megaparsec
import Text.Megaparsec.Char ( char, string ) -- megaparsec
import Text.Megaparsec.Char.Lexer ( decimal ) -- megaparsec

import Message ( Message(..), Pointer, messageToBuilder, messageParser', messageToHaskell, messageToPattern )
import Workspace ( WorkspaceId )

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
type FunEnv f v = M.Map f (VarEnv v) -- NOTE: Could possibly reduce this to a Var-to-Var mapping if all Values are labeled.
type PrimEnv s m p = M.Map p (s -> Value -> m Value)

type GoFn m s p f v = VarEnv v -> FunEnv f v -> s -> Exp p f v -> Kont1 s p f v -> m Value
type MatchFn m s p f v = s -> VarEnv v -> FunEnv f v -> f -> [Value] -> Kont1 s p f v -> m Value

data Kont1 s p f v
    = Done
    | PrimKont p s (Kont1 s p f v)
    | ArgKont [s] [Exp p f v] (Konts s p f v) -- Only for sequential.
    | SimpleKont (Konts s p f v) -- Essentially, SimpleKont k = ArgKont [] [] k
--     | NotifyKont (MVar Value) (Konts s p f v)

-- TODO: XXX Eliminate sequential stuff (i.e. ArgKont, PendKont, sequenceK). Have sequential or concurrent
-- be a matter of using threads versus sequentially pulling continuations from a queue.
data Konts s p f v
    = CallKont (FunEnv f v) f s (Kont1 s p f v)
    | PendKont Value (Konts s p f v) -- Only for sequential.
--    | JoinKont (Konts s p f v) -- TODO

applyKont1 :: (Monad m, Ord p, Ord f)
           => GoFn m s p f v
           -> MatchFn m s p f v
           -> VarEnv v
           -> FunEnv f v
           -> PrimEnv s m p
           -> Kont1 s p f v
           -> Value
           -> m Value
applyKont1 go match varEnv funEnv primEnv Done v = return v
applyKont1 go match varEnv funEnv primEnv (PrimKont p s k) v = do
    result <- (case M.lookup p primEnv of Just p' -> p') s v
    applyKont1 go match varEnv funEnv primEnv k result
applyKont1 go match varEnv funEnv primEnv (ArgKont ss es k) v = sequenceK go match varEnv funEnv ss es (PendKont v k)
applyKont1 go match varEnv funEnv primEnv (SimpleKont k) v = applyKonts match k [v]
-- applyKont1 go match varEnv funEnv primEnv (NotifyKont mvar k) v = do putMVar v; applyKont1 go match varEnv funEnv primEnv k v

applyKonts :: (Monad m, Ord f) => MatchFn m s p f v -> Konts s p f v -> [Value] -> m Value
applyKonts match (CallKont funEnv f s k) vs = match s (case M.lookup f funEnv of Just varEnv -> varEnv) funEnv f vs k
applyKonts match (PendKont v k) vs = applyKonts match k (v:vs)

-- TODO: XXX Can probably eliminate this by passing the FunEnv and Kont1 directly to match.
-- Only save continuations when we blockOnUser? Index Konts table by WorkspaceId and Name?

sequenceK :: (Monad m, Ord f) => GoFn m s p f v -> MatchFn m s p f v -> VarEnv v -> FunEnv f v -> [s] -> [Exp p f v] -> Konts s p f v -> m Value
sequenceK go match varEnv funEnv [] [] k = applyKonts match k []
sequenceK go match varEnv funEnv (s:ss) (e:es) k = go varEnv funEnv s e (ArgKont ss es k)
-- Intentionally incomplete.

concurrentlyK :: (Monad m) => GoFn m s p f v -> MatchFn m s p f v -> VarEnv v -> FunEnv f v -> [s] -> [Exp p f v] -> Konts s p f v -> m Value
concurrentlyK go match varEnv funEnv ss es k = undefined -- do -- TODO
--     mvars <- mapM (\_ -> newEmptyMVar) ss
--     zipWithM_ (\(s, e) mvar -> forkIO (go varEnv funEnv s e (NotifyKont mvar Done))) (zip ss es) mvars
--     applyKonts k =<< mapM takeMVar mvars

-- concurrentlyK acts k = applyKonts k =<< mapConcurrently ($ return) acts
-- Modulo dealing with exceptions, this is conceptually:
-- mvars <- mapM (\_ -> newEmptyMVar) acts  +------------------+ <-- notification continuations
-- zipWithM_ (\act mvar -> forkIO $         (act (putMVar mvar))) acts mvars
-- (k =<< mapM takeMVar mvars) <-- the join continuation

type ExecManyFn m s p f v = VarEnv v -> FunEnv f v -> s -> [Exp p f v] -> Konts s p f v -> m Value

evaluateExp :: (Ord p, Ord f, Ord v, Monad m)
            => ExecManyFn m s p f v
            -> MatchFn m s p f v
            -> (VarEnv v -> Value -> Value)
            -> PrimEnv s m p
            -> s
            -> Exp p f v
            -> Kont1 s p f v
            -> m Value
evaluateExp execMany match subst primEnv = evaluateExp' execMany match subst primEnv M.empty M.empty

evaluateExp' :: (Ord p, Ord f, Ord v, Monad m)
             => ExecManyFn m s p f v
             -> MatchFn m s p f v
             -> (VarEnv v -> Value -> Value)
             -> PrimEnv s m p
             -> VarEnv v
             -> FunEnv f v
             -> s
             -> Exp p f v
             -> Kont1 s p f v
             -> m Value
evaluateExp' execMany match subst primEnv = go
    where go varEnv funEnv s (Var x) k = applyKont1 go match varEnv funEnv primEnv k $! case M.lookup x varEnv of Just v -> v
          go varEnv funEnv s (Value v) k = applyKont1 go match varEnv funEnv primEnv k $ subst varEnv v
          go varEnv funEnv s (Prim p e) k = go varEnv funEnv s e (PrimKont p s k)
          go varEnv funEnv s (Call f es) k = execMany varEnv funEnv s es (CallKont funEnv f s k)
          go varEnv funEnv s (LetFun f body) k = go varEnv (M.insert f varEnv funEnv) s body k

type Var = Pointer
type Pattern = Message
type Primitive = Int

data Name = ANSWER | LOCAL !Int deriving ( Eq, Ord, Show )

type Exp' = Exp Primitive Name Var
type Kont1' = Kont1 WorkspaceId Primitive Name Var
type Konts' = Konts WorkspaceId Primitive Name Var

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

-- TODO: XXX Can fix fBuilder as nameToBuilder as I expect to have f = Name and s = WorkspaceId for keying in the database.
-- NOTE: All this isn't quite as bad as it looks, at least in the concurrent case. If CallKont gets represented by an
-- key for a database table, then Kont1 values will tend to be fairly small. Also ArgKont isn't necessary in the concurrent case.
--
-- The overall idea is that I'll have a database table of Konts keyed by WorkspaceId and Name that holds the contents of
-- CallKonts or maybe JoinKonts. (PendKont isn't needed in the concurrent case). Konts occurring in Kont1 values (which will
-- only be Done, PrimKont, SimpleKont[maybe?], and NotifyKont in the concurrent case), will be represented as WorkspaceId-Name pairs.
-- The actual representation of Kont1 values that are actually used will then be fairly small.
kont1ToBuilderDB' :: (p -> Builder) -> (f -> Builder) -> (v -> Builder) -> (f -> [([Pattern], Exp p f v)]) -> Kont1 WorkspaceId p f v -> Builder
kont1ToBuilderDB' pBuilder fBuilder vBuilder alternativesFor = go
    where go Done = fromText "Done"
          go (PrimKont p s k) = fromText "(PrimKont " <> pBuilder p <> singleton ' ' <> T.decimal s <> singleton ' ' <> go k <> singleton ')'
          go (ArgKont ss es k) = fromText "(ArgKont ("
                              <> intersperseChar ' '  T.decimal ss <> fromText ") ("
                              <> intersperseChar ' ' (expToBuilder' pBuilder fBuilder vBuilder alternativesFor) es <> fromText ") "
                              <> kontsToBuilderDB pBuilder fBuilder vBuilder alternativesFor k <> singleton ')'
          go (SimpleKont k) = fromText "(SimpleKont " <> kontsToBuilderDB pBuilder fBuilder vBuilder alternativesFor k <> singleton ')'
          -- go (NotifyKont mvar k) =

kont1ToBuilderDB :: Kont1' -> Builder
kont1ToBuilderDB = kont1ToBuilderDB' T.decimal nameToBuilder T.decimal (const []) -- TODO: Should eliminate alternativesFor eventually.

{-
kont1ParserDB :: Parsec Void T.Text Kont1'
kont1ParserDB = (Done <$ string "Done")
            <|> (PrimKont <$> (string "(PrimKont" *> decimal) <*> (char ' ' *> decimal) <*> (char ' ' *> kont1ParserDB) <* char ')')
            <|> (ArgKont <$> (string "(ArgKont (" *> sepBy decimal (char ' '))
                         <*> (string ") (" *> sepBy expParserDB (char ' '))
                         <*> (string ") " *> kontsParserDB <* char ')'))
            <|> (SimpleKont <$> (string "(SimpleKont " *> kontsParserDB <* char ')'))
            -- <|> (NotifyKont <$> (string "(NotifyKont " *> nameParser <* char ' ') <*> (kontsParserDB <* char ')'))


kontsParserDB :: Parsec Void T.Text (Name, WorkspaceId)
kontsParserDB = flip (,) <$> (string "(CallKont " *> nameParser) <*> (char ' ' *> decimal) <* char ')'
-}

kontsToBuilderDB :: (p -> Builder) -> (f -> Builder) -> (v -> Builder) -> (f -> [([Pattern], Exp p f v)]) -> Konts WorkspaceId p f v -> Builder
kontsToBuilderDB _ fBuilder _ _ = go
    where go (CallKont funEnv f s k) = fromText "(CallKont " <> T.decimal s <> singleton ' ' <> fBuilder f <> singleton ')'
          -- go (JoinKont k) =
{-
-- TODO: Like below, this will probably parse the key representation instead.
kontsParserDB :: Parsec Void T.Text Konts'
kontsParserDB = (CallKont <$> (string "(CallKont " *> funEnvParser)
                          <*> (char ' ' *> nameParser)
                          <*> (char ' ' *> decimal)
                          <*> (char ' ' *> kont1ParserDB) <* char ')')
            <|> (PendKont <$> (string "(PendKont " *> messageParser') <*> (char ' ' *> kontsParserDB) <* char ')')
            -- <|> (JoinKont <$> (string "(JoinKont " *> messageParser') <*> (char ' ' *> kontsParserDB) <* char ')')
    where funEnvParser = undefined -- TODO

kontsToBuilderDB :: (p -> Builder) -> (f -> Builder) -> (v -> Builder) -> (f -> [([Pattern], Exp p f v)]) -> Konts WorkspaceId p f v -> Builder
kontsToBuilderDB pBuilder fBuilder vBuilder alternativesFor = go
    where funEnvToBuilder _ = fromText "TODO" -- TODO
          go (CallKont funEnv f s k) = fromText "(CallKont "                   -- TODO: Don't really want this. Maybe don't want this function as a whole.
                                    <> funEnvToBuilder funEnv <> singleton ' ' -- I want to refer to Konts by index, i.e. WorkspaceId and Name.
                                    <> fBuilder f <> singleton ' '
                                    <> T.decimal s <> singleton ' '
                                    <> kont1ToBuilderDB' pBuilder fBuilder vBuilder alternativesFor k <> singleton ')'
          go (PendKont v k) = fromText "(PendKont " <> messageToBuilder v <> singleton ' ' <> go k <> singleton ')'
          -- go (JoinKont k) =
-}
