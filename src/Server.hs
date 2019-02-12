{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Server where
import Control.Concurrent.Chan ( Chan, newChan, readChan, writeChan ) -- base
import Control.Concurrent.MVar ( MVar, newEmptyMVar, putMVar, takeMVar ) -- base
import Control.Monad ( join, when ) -- base
import Control.Monad.IO.Class ( liftIO ) -- base
import Data.Aeson ( ToJSON, FromJSON ) -- aeson
import Data.Either ( partitionEithers ) -- base
import Data.IORef ( IORef, newIORef, readIORef, writeIORef, atomicModifyIORef', modifyIORef' ) -- base
import qualified Data.Map as M -- containers
import Data.String ( fromString ) -- base
import qualified Data.Text as T -- text
import Data.Tuple ( swap ) -- base
import Database.SQLite.Simple ( Connection ) -- sqlite-simple
import GHC.Generics ( Generic ) -- ghc
import Servant ( (:<|>)(..), (:>), Capture, Server, Get, Post, Proxy(..), ReqBody, JSON, Raw, serveDirectoryWebApp ) -- servant-server
import Servant.Server ( serve, Application ) -- servant-server
import System.Timeout ( timeout ) -- base

import AutoInterpreter ( runM, spawnInterpreter )
import Exp ( Name(..), Exp(..) )
import Message ( Message(..), Pointer )
import Scheduler ( UserId, Event(..), SchedulerFn, SchedulerContext(..),
                   firstUserId, getWorkspace, createInitialWorkspace, normalize, generalize, relabelMessage, createWorkspace )
import SqliteAutoSchedulerContext ( makeSqliteAutoSchedulerContext' )
import SqliteSchedulerContext ( makeSqliteSchedulerContext )
import Workspace ( WorkspaceId, Workspace(..), emptyWorkspace )

data Result = OK | Error T.Text deriving ( Generic )

instance FromJSON Result
instance ToJSON Result

data User = User { userId :: UserId } deriving ( Generic )

instance FromJSON User
instance ToJSON User

type StaticAPI = "static" :> Raw
type CommandAPI = "view" :> ReqBody '[JSON] (User, WorkspaceId, [Message], Pointer) :> Post '[JSON] Result
             :<|> "reply" :> ReqBody '[JSON] (User, WorkspaceId, [Either Message Pointer], Message) :> Post '[JSON] Result
             :<|> "wait" :> ReqBody '[JSON] (User, WorkspaceId, [Either Message Pointer]) :> Post '[JSON] Result
type PointerAPI = "pointer" :> Capture "p" Pointer :> Get '[JSON] (Maybe Message)
type NextAPI = "next" :> ReqBody '[JSON] User :> Post '[JSON] (Maybe Workspace)
type JoinAPI = "join" :> Get '[JSON] User

type API = CommandAPI :<|> NextAPI :<|> JoinAPI :<|> PointerAPI

type OverallAPI = StaticAPI :<|> API

staticHandler :: Server StaticAPI
staticHandler = serveDirectoryWebApp "static"

pointerHandler :: (Pointer -> IO Message) -> Server PointerAPI
pointerHandler deref ptr = liftIO $ do
    print ("Pointer", ptr) -- DELETEME
    Just <$> deref ptr -- TODO: Catch error and return Nothing.

-- TODO: Track users that have joined and only accept requests from previously seen users.
-- This will allow us to trivially ignore requests from users who've been falsely suspected
-- of abandoning a workspace, and will allow a new instance to invalidate any outstanding
-- work of a previous instance. (This is admittedly not a very seamless transition, but it's
-- simple and should suffice for our purposes assuming it ever even becomes an issue. This
-- would require user IDs to be unique across instances, e.g. UUIDs.)
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

commandHandler :: (UserId -> WorkspaceId -> [Event] -> IO ()) -> Server CommandAPI
commandHandler reply = viewHandler :<|> replyHandler :<|> waitHandler
    where viewHandler (User userId, workspaceId, msgs, ptr) = liftIO $ do
            print ("View", userId, workspaceId, msgs, ptr) -- DELETEME
            OK <$ reply userId workspaceId (map Create msgs ++ [Expand ptr])
          replyHandler (User userId, workspaceId, msgOrPtrs, msg) = liftIO $ do
            print ("Reply", userId, workspaceId, msgOrPtrs, msg) -- DELETEME
            OK <$ reply userId workspaceId (map (either Create Expand) msgOrPtrs ++ [Answer msg])
          waitHandler (User userId, workspaceId, msgOrPtrs) = liftIO $ do
            print ("Wait", userId, workspaceId, msgOrPtrs) -- DELETEME
            OK <$ reply userId workspaceId (map (either Create Expand) msgOrPtrs ++ [Submit])

overallHandler :: IORef UserId
               -> IO (Maybe WorkspaceId)
               -> (Pointer -> IO Message)
               -> (WorkspaceId -> IO Workspace)
               -> (UserId -> WorkspaceId -> [Event] -> IO ())
               -> Server OverallAPI
overallHandler userIdRef nextWorkspace deref lookupWorkspace reply
    = staticHandler
 :<|> commandHandler reply
 :<|> nextHandler lookupWorkspace nextWorkspace
 :<|> joinHandler userIdRef
 :<|> pointerHandler deref

-- For a web service, we're going to want to separate reading from the channel from giving a response.
-- Basically, when a web request comes in, we'll attempt to read a workspace to display from the channel (using
-- System.Timeout.timeout to handle the case when there are no available workspaces). Some policy will be required for users
-- that walk away without responding. Once we've decided a user isn't going to respond, we can reschedule the workspace
-- to a new user and discard any late response from the old user.
-- Probably use a keep-alive policy.
initServer :: Connection -> IO Application
initServer conn = do
    ctxt <- makeSqliteSchedulerContext conn

    requestChan <- newChan :: IO (Chan WorkspaceId)
    responseMVarsRef <- newIORef (M.empty :: M.Map WorkspaceId (MVar (UserId, Event)))
    drainingMVarsRef <- newIORef (M.empty :: M.Map WorkspaceId (MVar (UserId, Event)))

    let blockOnUser workspaceId = liftIO $ do
            mResponseMVar <- M.lookup workspaceId <$> readIORef drainingMVarsRef
            case mResponseMVar of
                Nothing -> do
                    responseMVar <- newEmptyMVar
                    modifyIORef' responseMVarsRef (M.insert workspaceId responseMVar)
                    writeChan requestChan workspaceId
                    takeMVar responseMVar
                Just responseMVar -> do
                    r@(_, resp) <- takeMVar responseMVar
                    case resp of
                        Submit -> do modifyIORef' drainingMVarsRef (M.delete workspaceId); return r
                        Answer _ -> do modifyIORef' drainingMVarsRef (M.delete workspaceId); return r
                        _ -> return r

        replyFromUser userId workspaceId [evt] = do
            Just responseMVar <- atomicModifyIORef' responseMVarsRef (swap . M.updateLookupWithKey (\_ _ -> Nothing) workspaceId)
            putMVar responseMVar (userId, evt)
        replyFromUser userId workspaceId (evt:evts) = do
            Just responseMVar <- atomicModifyIORef' responseMVarsRef (swap . M.updateLookupWithKey (\_ _ -> Nothing) workspaceId)
            modifyIORef' drainingMVarsRef (M.insert workspaceId responseMVar)
            putMVar responseMVar (userId, evt)
            mapM_ (putMVar responseMVar . (,) userId) evts -- NOTE: This assumes that a sort of protocol between the interpreter and this and will block forever if it is not met.

        begin = do
            initWorkspace <- getWorkspace ctxt =<< createInitialWorkspace ctxt
            return (identity initWorkspace, LetFun ANSWER (Call ANSWER [Value (question initWorkspace)]))

    doneRef <- newIORef True
    let nextWorkspace = do
            isDone <- atomicModifyIORef' doneRef (\d -> (False, d))
            when isDone $ do
                autoCtxt <- makeSqliteAutoSchedulerContext' ctxt
                () <$ runM (spawnInterpreter blockOnUser (liftIO begin) (liftIO $ writeIORef doneRef True) False autoCtxt) 0
            timeout 10000000 (readChan requestChan) -- Timeout after 10 seconds.

    userIdRef <- liftIO $ newIORef firstUserId

    return $ serve (Proxy :: Proxy OverallAPI) (overallHandler userIdRef nextWorkspace (dereference ctxt) (getWorkspace ctxt) replyFromUser)
