module Util ( toText, invertMap, Lock, newLock, withLock ) where
import Control.Concurrent.MVar ( MVar, newMVar, newEmptyMVar, putMVar, takeMVar ) -- base
import Control.Exception ( bracket_ ) -- base
import Data.Text.Lazy.Builder ( Builder, toLazyText ) -- text
import Data.Text.Lazy ( toStrict ) -- text
import Data.Tuple ( swap ) -- base
import qualified Data.Text as S ( Text ) -- text
import qualified Data.Map as M -- containers

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
