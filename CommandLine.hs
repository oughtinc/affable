{-# LANGUAGE OverloadedStrings #-}
module CommandLine where
import Data.Bifunctor ( first ) -- base
import Data.Foldable ( forM_ ) -- base
import Data.Text ( Text ) -- text
import Data.Void ( Void ) -- base
import qualified Data.Text.IO as T -- text
import System.Console.ANSI ( clearScreen ) -- ansi-terminal
import System.IO ( hFlush, stdout ) -- base
-- import Text.Megaparsec ( ParseErrorBundle, parse, errorBundlePretty ) -- megaparsec 7.0
import Text.Megaparsec ( ParseError, parse, parseErrorPretty ) -- megaparsec 6.5

import Command ( Command(..), commandParser )
import Message ( Message(..), messageToBuilder, expandPointers )
import Scheduler ( UserId, Event, SchedulerFn )
import qualified Scheduler as Event ( Event (..) )
import Util ( toText )
import Workspace ( Workspace(..), emptyWorkspace )

putMessage :: Message -> IO ()
putMessage = T.putStr . toText . messageToBuilder

putMessageLn :: Message -> IO ()
putMessageLn = T.putStrLn . toText . messageToBuilder

-- readCommand :: IO (Either (Text, ParseErrorBundle Text Void) Command) -- megaparsec 7.0
readCommand :: IO (Either (Text, ParseError Char Void) Command) -- megaparsec 6.5
readCommand = do
    l <- T.getLine
    return $ first ((,) l) (parse commandParser "" l)

renderWorkspace :: Workspace -> IO ()
renderWorkspace w = do
    T.putStr "Question: "
    let expand = expandPointers (expandedPointers w) -- TODO: Do renumbering.
    putMessageLn $ expand (question w)
    if null (subQuestions w) then return () else T.putStrLn "Subquestions:"
    forM_ (zip [1..] $ subQuestions w) $ \(i, (q, ma)) -> do
        putStr ("  " ++ show i ++ ". ")
        putMessageLn (expand q)
        case ma of
            Nothing -> return ()
            Just a -> do
                putStr "    Answer: "
                putMessageLn (expand a)
    if null (messageHistory w) then return () else T.putStrLn "Messages:"
    forM_ (zip [1..] $ messageHistory w) $ \(i, m) -> do
        putStr ("  " ++ show i ++ ". ")
        putMessageLn (expand m)

commandLineInteraction :: SchedulerFn -> IO ()
commandLineInteraction scheduler = do
    let initWorkspace = emptyWorkspace (Text "What is your question?")
    renderWorkspace initWorkspace
    go initWorkspace
  where userId = 0 :: UserId
        go ws = do
          T.putStr "> "
          hFlush stdout
          eCmd <- readCommand
          clearScreen
          case eCmd of
              Left (line, bundle) -> do
                  if line == "exit" then -- Probably make this case-insensitive and not sensitive to extra whitespace.
                      return ()
                    else do
                      -- putStr (errorBundlePretty bundle) -- megaparsec 7.0
                      putStr (parseErrorPretty bundle) -- megaparsec 6.5
                      go ws
              Right cmd -> do
                  mWorkspace <- scheduler userId ws (commandToEvent cmd)
                  case mWorkspace of
                      Nothing -> return ()
                      Just ws -> do
                          renderWorkspace ws
                          go ws

-- TODO: Is there any need to separate these, i.e. Command and Event?
commandToEvent :: Command -> Event
commandToEvent (Ask msg) = Event.Create msg
commandToEvent (Reply msg) = Event.Answer msg
commandToEvent (View p) = Event.Expand p
commandToEvent (Send addr msg) = Event.Send (fromIntegral addr) msg
