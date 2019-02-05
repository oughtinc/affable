module Util ( toText, invertMap, Lock, newLock, withLock, parseMap, mapToBuilder, parseUnsafe ) where
import Control.Concurrent.MVar ( MVar, newMVar, newEmptyMVar, putMVar, takeMVar ) -- base
import Control.Exception ( bracket_ ) -- base
import Data.List ( intersperse ) -- base
import qualified Data.Map as M -- containers
import qualified Data.Text as S ( Text ) -- text
import Data.Text.Lazy.Builder ( Builder, toLazyText, singleton ) -- text
import Data.Text.Lazy ( toStrict ) -- text
import Data.Tuple ( swap ) -- base
import Data.Void ( Void ) -- base
import Text.Megaparsec ( Parsec, sepBy, parse ) -- megaparsec
import Text.Megaparsec.Char ( char ) -- megaparsec

toText :: Builder -> S.Text
toText = toStrict . toLazyText

invertMap :: (Ord v) => M.Map k v -> M.Map v k
invertMap = M.fromList . map swap . M.assocs

type Lock = (IO (), IO ())

newLock :: IO Lock
newLock = do
    lockVar <- newMVar () :: IO (MVar ()) -- Initially unlocked.
    return (takeMVar lockVar, putMVar lockVar ())

withLock :: Lock -> IO a -> IO a
withLock (lock, unlock) = bracket_ lock unlock

mapToBuilder :: (k -> Builder) -> (v -> Builder) ->  M.Map k v -> Builder
mapToBuilder kBuilder vBuilder m = singleton '[' <>
    mconcat (intersperse (singleton ',') $ map (\(k, v) -> singleton '(' <> kBuilder k <> singleton ',' <> vBuilder v <> singleton ')' ) (M.toList m))
    <> singleton ']'

parseMap :: (Ord k) => Parsec Void S.Text k -> Parsec Void S.Text v -> Parsec Void S.Text (M.Map k v)
parseMap parseKey parseValue = fmap M.fromList (char '[' *> (parseEntry `sepBy` char ',') <* char ']')
    where parseEntry = (,) <$> (char '(' *> parseKey) <*> (char ',' *> parseValue) <* char ')'

parseUnsafe :: Parsec Void S.Text a -> S.Text -> a
parseUnsafe p t = case parse p "" t of Right x -> x
