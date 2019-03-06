{-# LANGUAGE OverloadedStrings #-}
module Util ( Lock, Queue, Counter, uuidToBuilder, parseUUID, newCounter, increment, toText, invertMap,
              newLock, withLock, newQueue, enqueueAsync, enqueueSync, closeQueue, parseMap, mapToBuilder, parseUnsafe ) where
import Control.Concurrent ( forkIO ) -- base
import Control.Concurrent.STM ( atomically ) -- stm
import Control.Concurrent.STM.TChan ( TChan, readTChan, writeTChan, newTChanIO ) -- stm
import Control.Concurrent.STM.TMVar ( TMVar, takeTMVar, putTMVar, newTMVarIO, newEmptyTMVarIO ) -- stm
import Control.Exception ( bracket_ ) -- base
import Control.Monad ( replicateM ) -- base
import Data.IORef ( IORef, newIORef, atomicModifyIORef' ) -- base
import Data.Int ( Int64 ) -- base
import Data.List ( intersperse ) -- base
import qualified Data.Map as M -- containers
import Data.String ( fromString ) -- base
import qualified Data.Text as S ( Text ) -- text
import Data.Text.Lazy.Builder ( Builder, toLazyText, singleton, fromText ) -- text
import Data.Text.Lazy ( toStrict ) -- text
import Data.Tuple ( swap ) -- base
import qualified Data.UUID as UUID -- uuid
import Data.Void ( Void ) -- base
import Text.Megaparsec ( Parsec, sepBy, parse, parseErrorPretty, (<?>) ) -- megaparsec
import Text.Megaparsec.Char ( char, hexDigitChar ) -- megaparsec

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

type Queue = TChan (Either (IO ()) (IO ()))

newQueue :: IO Queue
newQueue = do
    actionChan <- newTChanIO
    forkIO $ do
        let loop (Right action) = do -- TODO: Do something to prevent application exceptions from killing this thread.
                action
                atomically (readTChan actionChan) >>= loop
            loop (Left action) = do
                action
        atomically (readTChan actionChan) >>= loop
    return actionChan

enqueueAsync :: Queue -> IO () -> IO ()
enqueueAsync q action = atomically $ writeTChan q (Right action)

enqueueSync :: Queue -> IO a -> IO a
enqueueSync q action = do
    resultTMVar <- newEmptyTMVarIO
    enqueueAsync q (action >>= atomically . putTMVar resultTMVar)
    atomically $ takeTMVar resultTMVar

closeQueue :: Queue -> IO ()
closeQueue q = do
    resultTMVar <- newEmptyTMVarIO
    atomically $ writeTChan q (Left (atomically $ putTMVar resultTMVar ()))
    atomically $ takeTMVar resultTMVar

type Counter = IORef Int64

newCounter :: Int64 -> IO Counter
newCounter = newIORef

increment :: Counter -> IO Int64
increment c = atomicModifyIORef' c (\n -> (n+1, n))

uuidToBuilder :: UUID.UUID -> Builder
uuidToBuilder = fromText . UUID.toText

parseUUID :: Parsec Void S.Text UUID.UUID
parseUUID = (do
    p1 <- fromString <$> replicateM 8 hexDigitChar
    p2 <- fromString <$> (char '-' *> replicateM 4 hexDigitChar)
    p3 <- fromString <$> (char '-' *> replicateM 4 hexDigitChar)
    p4 <- fromString <$> (char '-' *> replicateM 4 hexDigitChar)
    p5 <- fromString <$> (char '-' *> replicateM 12 hexDigitChar)
    Just uuid <- return $ UUID.fromText (mconcat [p1,"-",p2,"-",p3,"-",p4,"-",p5])
    return uuid) <?> "UUID"
