module Util ( Lock, Queue,
              toText, invertMap, newLock, withLock, newQueue, enqueueAsync, enqueueSync, parseMap, mapToBuilder, parseUnsafe ) where
import Control.Concurrent ( forkIO ) -- base
import Control.Concurrent.STM ( atomically ) -- stm
import Control.Concurrent.STM.TChan ( TChan, readTChan, writeTChan, newTChanIO ) -- stm
import Control.Concurrent.STM.TMVar ( TMVar, takeTMVar, putTMVar, newTMVarIO, newEmptyTMVarIO ) -- stm
import Control.Exception ( bracket_ ) -- base
import Control.Monad ( forever ) -- base
import Data.List ( intersperse ) -- base
import qualified Data.Map as M -- containers
import qualified Data.Text as S ( Text ) -- text
import Data.Text.Lazy.Builder ( Builder, toLazyText, singleton ) -- text
import Data.Text.Lazy ( toStrict ) -- text
import Data.Tuple ( swap ) -- base
import Data.Void ( Void ) -- base
import Text.Megaparsec ( Parsec, sepBy, parse, parseErrorPretty ) -- megaparsec
import Text.Megaparsec.Char ( char ) -- megaparsec

toText :: Builder -> S.Text
toText = toStrict . toLazyText

invertMap :: (Ord v) => M.Map k v -> M.Map v k
invertMap = M.fromList . map swap . M.assocs

-- This is currently unused.
type Lock = (IO (), IO ())

newLock :: IO Lock
newLock = do
    lockVar <- newTMVarIO () :: IO (TMVar ()) -- Initially unlocked.
    return (atomically $ takeTMVar lockVar, atomically $ putTMVar lockVar ())

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
parseUnsafe p t = case parse p "" t of Right x -> x; Left bundle -> error (parseErrorPretty bundle)

type Queue = TChan (IO ())

-- TODO: There should probably be a closeQueue operation. We could change Queue to
-- be a TChan (Maybe (IO ())) and closeQueue just sends Nothing which ends the forever loop.
newQueue :: IO Queue
newQueue = do
    actionChan <- newTChanIO
    forkIO $ do
        forever $ do -- TODO: Do something to prevent application exceptions from killing this thread.
            action <- atomically $ readTChan actionChan
            action
    return actionChan

enqueueAsync :: Queue -> IO () -> IO ()
enqueueAsync q action = atomically $ writeTChan q action

enqueueSync :: Queue -> IO a -> IO a
enqueueSync q action = do
    resultTMVar <- newEmptyTMVarIO
    enqueueAsync q (action >>= atomically . putTMVar resultTMVar)
    atomically $ takeTMVar resultTMVar
