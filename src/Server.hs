{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Server where
import Control.Concurrent.Chan ( Chan, newChan, readChan, writeChan ) -- base
import Control.Concurrent.MVar ( MVar, newEmptyMVar, putMVar, takeMVar ) -- base
import Control.Monad ( join ) -- base
import Control.Monad.IO.Class ( liftIO ) -- base
import Data.Aeson ( ToJSON, FromJSON ) -- aeson
import Data.Either ( partitionEithers ) -- base
import Data.IORef ( IORef, newIORef, readIORef, atomicModifyIORef', modifyIORef' ) -- base
import qualified Data.Map as M -- containers
import qualified Data.Text as T -- text
import Data.Tuple ( swap ) -- base
import Data.String ( fromString ) -- base
import Database.SQLite.Simple ( Connection ) -- sqlite-simple
import GHC.Generics ( Generic ) -- ghc
import Servant ( (:<|>)(..), (:>), Server, Get, Post, Proxy(..), ReqBody, JSON, Raw, serveDirectoryWebApp ) -- servant-server
import Servant.Server ( serve, Application ) -- servant-server
import System.Timeout ( timeout ) -- base

import AutoInterpreter ( spawnInterpreter )
import AutoScheduler ( schedulerContext )
import Exp ( Name(..), Exp(..) )
import Message ( Message(..), Pointer )
import Scheduler ( SchedulerFn, UserId, Event(..), getWorkspace, createInitialWorkspace, normalize, generalize, relabelMessage, createWorkspace )
import SqliteAutoSchedulerContext ( makeSqliteAutoSchedulerContext )
import Workspace ( WorkspaceId, Workspace(..), emptyWorkspace )

data Result = OK | Error T.Text deriving ( Generic )

instance FromJSON Result
instance ToJSON Result

data User = User { userId :: UserId } deriving ( Generic )

instance FromJSON User
instance ToJSON User

type StaticAPI = "static" :> Raw
type CommandAPI = "view" :> ReqBody '[JSON] (User, WorkspaceId, Pointer) :> Post '[JSON] Result
             :<|> "reply" :> ReqBody '[JSON] (User, WorkspaceId, Message) :> Post '[JSON] Result
             :<|> "wait" :> ReqBody '[JSON] (User, WorkspaceId, [Message]) :> Post '[JSON] Result
type NextAPI = "next" :> ReqBody '[JSON] User :> Post '[JSON] (Maybe Workspace)
type JoinAPI = "join" :> Get '[JSON] User

type API = CommandAPI :<|> NextAPI :<|> JoinAPI

type OverallAPI = StaticAPI :<|> API

staticHandler :: Server StaticAPI
staticHandler = serveDirectoryWebApp "static"

joinHandler :: IORef UserId -> Server JoinAPI
joinHandler userIdRef = liftIO $ do
    putStrLn "Join" -- DELETEME
    atomicModifyIORef' userIdRef (\n -> (n+1, User n))

-- TODO: Cache the user's workspace for a time so refreshing and clicking next doesn't lose the workspace.
-- Then determine a policy for giving up on a user response and writing the workspace back into the channel
-- for someone else.
nextHandler :: (WorkspaceId -> IO Workspace) -> IO (Maybe WorkspaceId) -> Server NextAPI
nextHandler lookupWorkspace nextWorkspace (User userId) = liftIO $ do
    print ("Next", userId) -- DELETEME
    mWorkspaceId <- nextWorkspace
    case mWorkspaceId of
        Just workspaceId -> Just <$> lookupWorkspace workspaceId
        Nothing -> return Nothing

commandHandler :: (WorkspaceId -> [Event] -> IO ()) -> Server CommandAPI
commandHandler reply = viewHandler :<|> replyHandler :<|> waitHandler
    where viewHandler (User userId, workspaceId, ptr) = liftIO $ do
            print ("View", userId, workspaceId, ptr) -- DELETEME
            OK <$ reply workspaceId [Expand ptr]
          replyHandler (User userId, workspaceId, msg) = liftIO $ do
            print ("Reply", userId, workspaceId, msg) -- DELETEME
            OK <$ reply workspaceId [Answer msg]
          waitHandler (User userId, workspaceId, msgs) = liftIO $ do
            print ("Wait", userId, workspaceId, msgs) -- DELETEME
            OK <$ reply workspaceId (map Create msgs ++ [Submit])

overallHandler :: IORef UserId -> IO (Maybe WorkspaceId) -> (WorkspaceId -> IO Workspace) -> (WorkspaceId -> [Event] -> IO ()) -> Server OverallAPI
overallHandler userIdRef nextWorkspace lookupWorkspace reply
    = staticHandler :<|> commandHandler reply :<|> nextHandler lookupWorkspace nextWorkspace :<|> joinHandler userIdRef

-- For a web service, we're going to want to separate reading from the channel from giving a response.
-- Basically, when a web request comes in, we'll attempt to read a workspace to display from the channel (using
-- System.Timeout.timeout to handle the case when there are no available workspaces). When the user responds,
-- we can use the first two lines of replyFromUser below to send the result. Some policy will be required for users
-- that walk away without responding. Once we've decided a user isn't going to respond, we can reschedule the workspace
-- to a new user and discard any late response from the old user.
-- Probably use keep-alive policy.
initServer :: Connection -> IO Application
initServer conn = do
    autoCtxt <- makeSqliteAutoSchedulerContext conn
    let !ctxt = schedulerContext autoCtxt
    initWorkspace <- getWorkspace ctxt =<< createInitialWorkspace ctxt
    let !initWorkspaceId = identity initWorkspace

    requestChan <- newChan :: IO (Chan (Maybe WorkspaceId))
    responseMVarsRef <- newIORef (M.empty :: M.Map WorkspaceId (MVar Event))
    drainingMVarsRef <- newIORef (M.empty :: M.Map WorkspaceId (MVar Event))

    let blockOnUser True workspaceId = do
            Just responseMVar <- M.lookup workspaceId <$> readIORef drainingMVarsRef
            resp <- takeMVar responseMVar
            case resp of
                Submit -> do modifyIORef' drainingMVarsRef (M.delete workspaceId); return Submit
                _ -> return resp
        blockOnUser _ workspaceId = do
            responseMVar <- newEmptyMVar
            modifyIORef' responseMVarsRef (M.insert workspaceId responseMVar)
            writeChan requestChan (Just workspaceId)
            takeMVar responseMVar

        replyFromUser workspaceId [evt] = do
            Just responseMVar <- atomicModifyIORef' responseMVarsRef (swap . M.updateLookupWithKey (\_ _ -> Nothing) workspaceId)
            putMVar responseMVar evt
        replyFromUser workspaceId (evt:evts) = do
            Just responseMVar <- atomicModifyIORef' responseMVarsRef (swap . M.updateLookupWithKey (\_ _ -> Nothing) workspaceId)
            modifyIORef' drainingMVarsRef (M.insert workspaceId responseMVar)
            putMVar responseMVar evt
            mapM_ (putMVar responseMVar) evts -- NOTE: This assumes that a sort of protocol between the interpreter and this and will block forever if it is not met.

        begin = return (initWorkspaceId, LetFun ANSWER (Call ANSWER [Value (question initWorkspace)]))

    spawnInterpreter blockOnUser begin (writeChan requestChan Nothing) False autoCtxt

    let nextWorkspace = join <$> timeout 10000000 (readChan requestChan) -- Timeout after 10 seconds.

    userIdRef <- liftIO $ newIORef (0 :: UserId)

    return $ serve (Proxy :: Proxy OverallAPI) (overallHandler userIdRef nextWorkspace (getWorkspace ctxt) replyFromUser)
