{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module CommandLine where
import Data.Bifunctor ( first ) -- base
import Data.Foldable ( foldl', forM_ ) -- base
import qualified Data.Map as M -- containers
import Data.Text ( Text ) -- text
import Data.Void ( Void ) -- base
import qualified Data.Text.IO as T -- text
import System.Console.ANSI ( clearScreen ) -- ansi-terminal
import System.IO ( hFlush, stdout ) -- base
-- import Text.Megaparsec ( ParseErrorBundle, parse, errorBundlePretty ) -- megaparsec 7.0
import Text.Megaparsec ( ParseError, parse, parseErrorPretty ) -- megaparsec 6.5

import Command ( Command(..), commandParser )
import Message ( Message(..), PointerRemapping, messageToBuilder, expandPointers, renumberMessage, renumberAcc )
import Scheduler ( UserId, Event, SchedulerFn )
import qualified Scheduler as Event ( Event (..) )
import Util ( toText, invertMap )
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

renderWorkspace :: Workspace -> IO PointerRemapping
renderWorkspace w = do
    T.putStr "Question: "
    let expand = expandPointers (expandedPointers w)
    -- TODO: Avoid expanding messages twice.
    -- TODO: Figure out a scheme to get stable renumbering.
    let mapping = foldl' renumberAcc M.empty $ concat [
                        [expand $ question w],
                        concatMap (\(q, ma) -> expand q:maybe [] ((:[]) . expand) ma) (subQuestions w),
                        map expand $ messageHistory w
                    ]
    let expand' = renumberMessage mapping . expand
    putMessageLn $ expand' (question w)
    if null (subQuestions w) then return () else T.putStrLn "Subquestions:"
    forM_ (zip [1..] $ subQuestions w) $ \(i, (q, ma)) -> do
        putStr ("  " ++ show i ++ ". ")
        putMessageLn (expand' q)
        case ma of
            Nothing -> return ()
            Just a -> do
                putStr "    Answer: "
                putMessageLn (expand' a)
    if null (messageHistory w) then return () else T.putStrLn "Messages:"
    forM_ (zip [1..] $ messageHistory w) $ \(i, m) -> do
        putStr ("  " ++ show i ++ ". ")
        putMessageLn (expand' m)
    return $ invertMap mapping

commandLineInteraction :: SchedulerFn -> IO ()
commandLineInteraction scheduler = do
    let initWorkspace = emptyWorkspace (Text "What is your question?")
    mapping <- renderWorkspace initWorkspace
    go mapping initWorkspace
  where userId = 0 :: UserId
        go mapping ws = do
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
                      go mapping ws
              Right cmd -> do
                  mWorkspace <- scheduler userId ws (commandToEvent mapping cmd)
                  case mWorkspace of
                      Nothing -> return ()
                      Just ws -> do
                          mapping' <- renderWorkspace ws
                          go mapping' ws

commandToEvent :: PointerRemapping -> Command -> Event
commandToEvent mapping (Ask msg) = Event.Create $ renumberMessage mapping msg
commandToEvent mapping (Reply msg) = Event.Answer $ renumberMessage mapping msg
commandToEvent mapping (View p) = Event.Expand $ maybe p id (M.lookup p mapping)
commandToEvent mapping (Send addr msg) = Event.Send (fromIntegral addr) $ renumberMessage mapping msg
