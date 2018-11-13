{-# LANGUAGE OverloadedStrings #-}
module CommandLine where
import qualified Data.Text.IO as T -- text
import Text.Megaparsec ( parse, errorBundlePretty ) -- megaparsec

import Command ( Command(..), commandParser )
import Message ( Message(..), messageToBuilder )
import Util ( toText )
import Workspace

{-
data Workspace = Workspace {
    question :: Question,
    subQuestions :: [(Question, Maybe Answer)],
    messageHistory :: [Message], -- TODO: Do we want the history to include who the message was from?
    expandedPointers :: PointerEnvironment,
    time :: Time
    -- ... TODO
  } deriving ( Eq, Show, Generic )
-}

putMessage :: Message -> IO ()
putMessage = T.putStr . toText . messageToBuilder

putMessageLn :: Message -> IO ()
putMessageLn = T.putStrLn . toText . messageToBuilder

readCommand :: IO (Maybe Command)
readCommand = do
    l <- T.getLine
    case parse commandParser "" l of
        Left bundle -> do
            putStr (errorBundlePretty bundle)
            return Nothing
        Right cmd -> return (Just cmd)

renderWorkspace :: Workspace -> IO ()
renderWorkspace w = do
    T.putStr "Question: "
    putMessageLn $ question w
    -- TODO: Continue.
