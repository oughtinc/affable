{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Server where
import Control.Concurrent.Chan ( Chan, newChan, readChan, writeChan ) -- base
import Control.Concurrent.MVar ( MVar, newEmptyMVar, putMVar, takeMVar ) -- base
import Control.Monad.IO.Class ( liftIO ) -- base
import Data.Aeson ( ToJSON, FromJSON ) -- aeson
import Data.Bifunctor ( second ) -- base
import Data.Either ( partitionEithers ) -- base
import Data.IORef ( IORef, newIORef, readIORef, writeIORef, atomicModifyIORef', modifyIORef' ) -- base
import qualified Data.Map as M -- containers
import Data.String ( fromString ) -- base
import qualified Data.Text as T -- text
import Data.Tuple ( swap ) -- base
import Database.SQLite.Simple ( Connection ) -- sqlite-simple
import GHC.Generics ( Generic ) -- ghc
import Servant ( (:<|>)(..), (:>), QueryParam, Capture, Server, Get, Post, Proxy(..), ReqBody, JSON, Raw, serveDirectoryWebApp ) -- servant-server
import Servant.Server ( serve, Application ) -- servant-server
import System.Timeout ( timeout ) -- base

import AutoInterpreter ( runM, spawnInterpreter )
import Exp ( Name(..), Exp(..) )
import Message ( Message(..), Pointer )
import Scheduler ( SessionId, UserId, Event(..), SchedulerFn, SchedulerContext(..),
                   firstUserId, getWorkspace, createInitialWorkspace, normalize, generalize, relabelMessage, createWorkspace )
import SqliteAutoSchedulerContext ( makeSqliteAutoSchedulerContext' )
import SqliteSchedulerContext ( makeSqliteSchedulerContext )
import Workspace ( WorkspaceId, Workspace(..), emptyWorkspace )

data Response = OK | Error T.Text deriving ( Generic )

instance FromJSON Response
instance ToJSON Response

data User = User { userId :: UserId } deriving ( Generic )

instance FromJSON User
instance ToJSON User

type StaticAPI = "static" :> Raw
type CommandAPI = "view" :> ReqBody '[JSON] (User, WorkspaceId, [Message], Pointer) :> Post '[JSON] Response
             :<|> "reply" :> ReqBody '[JSON] (User, WorkspaceId, [Either Message Pointer], Message) :> Post '[JSON] Response
             :<|> "wait" :> ReqBody '[JSON] (User, WorkspaceId, [Either Message Pointer]) :> Post '[JSON] Response
type PointerAPI = "pointer" :> Capture "p" Pointer :> Get '[JSON] (Maybe Message)
type NextAPI = "next" :> ReqBody '[JSON] (User, Maybe SessionId) :> Post '[JSON] (Maybe (Workspace, SessionId))
type JoinAPI = "join" :> QueryParam "userId" UserId :> Get '[JSON] User

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
joinHandler :: (Maybe UserId -> IO User) -> Server JoinAPI
joinHandler makeUser oldUserId = liftIO $ do
    print ("Join", oldUserId) -- DELETEME
    makeUser oldUserId

-- TODO: Cache the user's workspace for a time so refreshing and clicking next doesn't lose the workspace.
-- Then determine a policy for giving up on a user response and writing the workspace back into the channel
-- for someone else.
nextHandler :: (WorkspaceId -> IO Workspace) -> (User -> Maybe SessionId -> IO (Maybe (WorkspaceId, SessionId))) -> Server NextAPI
nextHandler lookupWorkspace nextWorkspace (user@(User userId), mSessionId) = liftIO $ do
    print ("Next", userId, mSessionId) -- DELETEME
    mWorkspaceId <- nextWorkspace user mSessionId
    case mWorkspaceId of
        Just (workspaceId, sessionId) -> fmap (\ws -> Just (ws, sessionId)) (lookupWorkspace workspaceId)
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

overallHandler :: (Maybe UserId -> IO User)
               -> (User -> Maybe SessionId -> IO (Maybe (WorkspaceId, SessionId)))
               -> (Pointer -> IO Message)
               -> (WorkspaceId -> IO Workspace)
               -> (UserId -> WorkspaceId -> [Event] -> IO ())
               -> Server OverallAPI
overallHandler makeUser nextWorkspace deref lookupWorkspace reply
    = staticHandler
 :<|> commandHandler reply
 :<|> nextHandler lookupWorkspace nextWorkspace
 :<|> joinHandler makeUser
 :<|> pointerHandler deref

data SessionState = SessionState {
    sessionRequestChan :: Chan WorkspaceId,
    sessionResponseMVarsRef :: IORef (M.Map WorkspaceId (MVar (UserId, Event))),
    sessionDrainingMVarsRef :: IORef (M.Map WorkspaceId (MVar (UserId, Event))),
    sessionDoneRef :: IORef Bool }

-- For a web service, we're going to want to separate reading from the channel from giving a response.
-- Basically, when a web request comes in, we'll attempt to read a workspace to display from the channel (using
-- System.Timeout.timeout to handle the case when there are no available workspaces). Some policy will be required for users
-- that walk away without responding. Once we've decided a user isn't going to respond, we can reschedule the workspace
-- to a new user and discard any late response from the old user.
-- Probably use a keep-alive policy.
initServer :: Connection -> IO Application
initServer conn = do
    ctxt <- makeSqliteSchedulerContext conn

    userIdRef <- liftIO $ newIORef (M.empty :: M.Map UserId (M.Map SessionId (Maybe WorkspaceId)))
    sessionMapRef <- newIORef (M.empty :: M.Map SessionId SessionState)
    userSessionMapRef <- newIORef (M.empty :: M.Map UserId SessionId)

    let newSessionState = SessionState <$> newChan <*> newIORef M.empty <*> newIORef M.empty <*> newIORef False

        userSessionState userId = do
            userSessionMap  <- readIORef userSessionMapRef
            let !sessionId = case M.lookup userId userSessionMap of Just sessionId -> sessionId
            second (const sessionId) <$> sessionState sessionId

        sessionState sessionId = do
            sessionMap <- readIORef sessionMapRef
            case M.lookup sessionId sessionMap of
                Just ss -> return (ss, False)
                Nothing -> do
                    ss <- newSessionState
                    modifyIORef' sessionMapRef (M.insertWith (\_ old -> old) sessionId ss) -- Keep the existing SessionState if it already exists.
                    return (ss, True)

        blockOnUser sessionId workspaceId = liftIO $ do
            (ss, False) <- sessionState sessionId -- Should never be a new SessionState.
            let !responseMVarsRef = sessionResponseMVarsRef ss
                !drainingMVarsRef = sessionDrainingMVarsRef ss
                !requestChan = sessionRequestChan ss
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

        -- TODO: Check if userID is in userIdRef. If not, then ignore.
        replyFromUser userId workspaceId [evt] = do
            (ss, sessionId) <- userSessionState userId
            let !responseMVarsRef = sessionResponseMVarsRef ss
            Just responseMVar <- atomicModifyIORef' responseMVarsRef (swap . M.updateLookupWithKey (\_ _ -> Nothing) workspaceId)
            updateUserSession userId sessionId Nothing
            putMVar responseMVar (userId, evt)
        replyFromUser userId workspaceId (evt:evts) = do
            (ss, sessionId) <- userSessionState userId
            let !responseMVarsRef = sessionResponseMVarsRef ss
                !drainingMVarsRef = sessionDrainingMVarsRef ss
            Just responseMVar <- atomicModifyIORef' responseMVarsRef (swap . M.updateLookupWithKey (\_ _ -> Nothing) workspaceId)
            modifyIORef' drainingMVarsRef (M.insert workspaceId responseMVar)
            updateUserSession userId sessionId Nothing
            putMVar responseMVar (userId, evt)
            mapM_ (putMVar responseMVar . (,) userId) evts -- NOTE: This assumes that a sort of protocol between the interpreter and this and will block forever if it is not met.

        begin = do
            initWorkspace <- getWorkspace ctxt =<< createInitialWorkspace ctxt
            let !q = case question initWorkspace of LabeledStructured _ ms -> Structured ms; m -> m
            return (identity initWorkspace, LetFun ANSWER (Call ANSWER [Value q]))

        activeWorkspaceId userId sessionId = do
            uIds <- readIORef userIdRef
            case M.lookup userId uIds of
                Nothing -> return Nothing
                Just sessions -> return $ M.lookup sessionId sessions

        updateUserSession userId sessionId mWorkspaceId = do
            modifyIORef' userIdRef (M.insertWith M.union userId (M.singleton sessionId mWorkspaceId))

        nextWorkspace user@(User userId) mSessionId = do
            sessionId <- makeSession user mSessionId
            mmWorkspaceId <- activeWorkspaceId userId sessionId
            case mmWorkspaceId of
                Just mWorkspaceId@(Just _) -> do
                    return $ fmap (\wsId -> (wsId, sessionId)) mWorkspaceId
                _ -> do -- Nothing means the user hasn't been seen before in this session, but we should just add them anyway, so no problem.
                    (ss, isNew) <- sessionState sessionId
                    if isNew then do
                        let !doneRef = sessionDoneRef ss
                            !requestChan = sessionRequestChan ss
                        autoCtxt <- makeSqliteAutoSchedulerContext' sessionId ctxt
                        runM (spawnInterpreter (blockOnUser sessionId) (liftIO begin) (liftIO $ writeIORef doneRef True) False autoCtxt) 0
                        mWorkspaceId <- timeout 10000000 (readChan requestChan) -- Timeout after 10 seconds.
                        updateUserSession userId sessionId mWorkspaceId
                        return $ fmap (\wsId -> (wsId, sessionId)) mWorkspaceId
                      else do
                        let !doneRef = sessionDoneRef ss
                        isDone <- readIORef doneRef
                        if isDone then do -- TODO: We can clear out the session state for the old session ID now.
                            nextWorkspace user Nothing
                          else do
                            let !requestChan = sessionRequestChan ss
                            mWorkspaceId <- timeout 10000000 (readChan requestChan) -- Timeout after 10 seconds.
                            updateUserSession userId sessionId mWorkspaceId
                            return $ fmap (\wsId -> (wsId, sessionId)) mWorkspaceId

        makeUser Nothing = do
            atomicModifyIORef' userIdRef $ \uIds ->
                let !uId = maybe firstUserId (succ . fst) (M.lookupMax uIds)
                in (M.insert uId M.empty uIds, User uId)
        makeUser (Just oldUserId) = do
            uIds <- readIORef userIdRef
            case M.lookup oldUserId uIds of
                Just _ -> return (User oldUserId) -- TODO: Only keep old user ID if it was in this run.
                                                  -- Need User ID's to be more unique before they can just be taken for granted.
                Nothing -> makeUser Nothing

        makeSession u@(User userId) mSessionId = do
            sessionId <- newSession ctxt mSessionId
            modifyIORef' userSessionMapRef (M.insert userId sessionId)
            return sessionId

    return $ serve (Proxy :: Proxy OverallAPI) (overallHandler makeUser nextWorkspace (dereference ctxt) (getWorkspace ctxt) replyFromUser)
