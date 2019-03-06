{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module CommandLine where
import Control.Monad.IO.Class ( liftIO ) -- base
import Data.Bifunctor ( first ) -- base
import Data.Foldable ( foldl', forM_ ) -- base
import Data.List ( isPrefixOf ) -- base
import qualified Data.Map as M -- containers
import Data.String ( fromString ) -- base
import Data.Text ( Text ) -- text
import Data.Void ( Void ) -- base
import qualified Data.Text.IO as T -- text
-- import System.Console.ANSI ( clearScreen, setCursorPosition ) -- ansi-terminal
import System.Console.Haskeline ( InputT, CompletionFunc, outputStr, outputStrLn,
                                  setComplete, completeWord, simpleCompletion, defaultSettings, runInputT, getInputLine ) -- haskeline
-- import System.IO ( hFlush, stdout ) -- base
-- import Text.Megaparsec ( ParseErrorBundle, parse, errorBundlePretty ) -- megaparsec 7.0
import Text.Megaparsec ( ParseError, parse, parseErrorPretty ) -- megaparsec 6.5

import Command ( Command(..), commandParser )
import Message ( Message(..), PointerRemapping, messageToBuilder, expandPointers, renumberMessage, renumberMessage', renumberAcc )
import Scheduler ( Event, SchedulerFn, newUserId )
import qualified Scheduler as Event ( Event (..) )
import Util ( toText, invertMap )
import Workspace ( Workspace(..) )

putMessageLn :: Message -> InputT IO ()
putMessageLn = liftIO . T.putStrLn . toText . messageToBuilder

-- readCommand :: InputT IO (Either (Text, ParseErrorBundle Text Void) Command) -- megaparsec 7.0
readCommand :: InputT IO (Either (Text, ParseError Char Void) Command) -- megaparsec 6.5
readCommand = do
    l <- maybe "exit" fromString <$> getInputLine "> "
    return $ first ((,) l) (parse commandParser "" l)

renderWorkspace :: Workspace -> InputT IO PointerRemapping
renderWorkspace w = do
    outputStr "Question: "
    let expand = expandPointers (expandedPointers w)
    -- TODO: Avoid expanding messages twice.
    let mapping = foldl' renumberAcc M.empty $ concat [
                        [expand $ question w],
                        concatMap (\(_, q, ma) -> expand q:maybe [] ((:[]) . expand) ma) (subQuestions w),
                        map expand $ messageHistory w
                    ]
    let expand' = maybe (error "renderWorkspace: Shouldn't happen.") id . renumberMessage mapping . expand
    putMessageLn $ expand' (question w)
    if null (subQuestions w) then return () else outputStrLn "Subquestions:"
    forM_ (zip [1..] $ subQuestions w) $ \(i, (_, q, ma)) -> do
        outputStr ("  " ++ show i ++ ". ")
        putMessageLn (expand' q)
        case ma of
            Nothing -> return ()
            Just a -> do
                outputStr "    Answer: "
                putMessageLn (expand' a)
    if null (messageHistory w) then return () else outputStrLn "Messages:"
    forM_ (zip [1..] $ messageHistory w) $ \(i, m) -> do
        outputStr ("  " ++ show i ++ ". ")
        putMessageLn (expand' m)
    return $ invertMap mapping

-- TODO: Could add valid pointers...
completionFunc :: CompletionFunc IO
completionFunc = completeWord Nothing "" $ \s -> return $ map simpleCompletion $ filter (s `isPrefixOf`) table
    where table = ["ask", "reply", "view", "wait", "send"]

commandLineInteraction :: Workspace -> SchedulerFn -> IO ()
commandLineInteraction initWorkspace scheduler = do
    userId <- newUserId
    runInputT (setComplete completionFunc defaultSettings) $ do
        mWorkspace <- liftIO $ scheduler userId initWorkspace Event.Init
        case mWorkspace of
            Nothing -> return ()
            Just ws -> do
                mapping' <- renderWorkspace ws
                go userId mapping' ws
  where go userId mapping ws = do
          eCmd <- readCommand
          case eCmd of
              Left (line, bundle) -> do
                  if line == "exit" then -- Probably make this case-insensitive and not sensitive to extra whitespace.
                      return ()
                    else do
                      -- outputStr (errorBundlePretty bundle) -- megaparsec 7.0
                      outputStr (parseErrorPretty bundle) -- megaparsec 6.5
                      go userId mapping ws
              Right cmd -> do
                  case commandToEvent mapping cmd of
                    Nothing -> do
                        outputStrLn "Reference to undefined pointer." -- TODO: Better wording.
                        go userId mapping ws
                    Just evt -> do
                      -- liftIO $ clearScreen >> setCursorPosition 0 0 >> hFlush stdout
                      mWorkspace <- liftIO $ scheduler userId ws evt
                      case mWorkspace of
                          Nothing -> return ()
                          Just ws -> do
                              mapping' <- renderWorkspace ws
                              go userId mapping' ws

commandToEvent :: PointerRemapping -> Command -> Maybe Event
commandToEvent mapping (Ask msg) = Just (Event.Create $ renumberMessage' mapping msg)
commandToEvent mapping (Reply msg) = Just (Event.Answer $ renumberMessage' mapping msg)
commandToEvent mapping (View p) = Event.Expand <$> M.lookup p mapping
commandToEvent mapping (Send addr msg) = Just (Event.Send addr $ renumberMessage' mapping msg)
commandToEvent mapping Wait = Just Event.Submit
