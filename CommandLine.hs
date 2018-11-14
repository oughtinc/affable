{-# LANGUAGE OverloadedStrings #-}
module CommandLine where
import Data.Bifunctor ( first ) -- base
import Data.Text ( Text ) -- text
import Data.Void ( Void ) -- base
import qualified Data.Text.IO as T -- text
import System.IO ( hFlush, stdout ) -- base
import Text.Megaparsec ( ParseErrorBundle, parse, errorBundlePretty ) -- megaparsec

import Command ( Command(..), commandParser )
import Message ( Message(..), messageToBuilder )
import Scheduler ( UserId, Event, SchedulerFn )
import qualified Scheduler as Event ( Event (..) )
import Util ( toText )
import Workspace

putMessage :: Message -> IO ()
putMessage = T.putStr . toText . messageToBuilder

putMessageLn :: Message -> IO ()
putMessageLn = T.putStrLn . toText . messageToBuilder

readCommand :: IO (Either (Text, ParseErrorBundle Text Void) Command)
readCommand = do
    l <- T.getLine
    return $ first ((,) l) (parse commandParser "" l)

renderWorkspace :: Workspace -> IO ()
renderWorkspace w = do
    T.putStr "Question: "
    putMessageLn $ question w
    -- TODO: Continue.

commandLineInteraction :: SchedulerFn -> IO ()
commandLineInteraction scheduler = do
    -- TODO: Insert some initial greeting.
    T.putStrLn "What is your question?"
    go 0
  where userId = 0 :: UserId
        go workspaceId = do
          T.putStr "> "
          hFlush stdout
          eCmd <- readCommand
          case eCmd of
              Left (line, bundle) -> do
                  if line == "exit" then -- Probably make this case-insensitive and not sensitive to extra whitespace.
                      return ()
                    else do
                      putStr (errorBundlePretty bundle)
                      go workspaceId
              Right cmd -> do
                  mWorkspace <- scheduler userId workspaceId (commandToEvent cmd)
                  case mWorkspace of
                      Nothing -> return ()
                      Just ws -> do
                          renderWorkspace ws
                          go (identity ws)

-- TODO: Is there any need to separate these, i.e. Command and Event?
commandToEvent :: Command -> Event
commandToEvent (Ask msg) = Event.Create msg
commandToEvent (Reply msg) = Event.Answer msg
commandToEvent (View p) = Event.Expand p
commandToEvent (Send addr msg) = Event.Send (fromIntegral addr) msg
