{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Server where
import Control.Concurrent.STM ( atomically ) -- stm
import Control.Concurrent.STM.TChan ( TChan, newTChanIO, readTChan, writeTChan ) -- stm
import Control.Concurrent.STM.TMVar ( TMVar, newEmptyTMVarIO, putTMVar, takeTMVar ) -- stm
import Control.Monad.IO.Class ( liftIO ) -- base
import Data.Aeson ( ToJSON, FromJSON ) -- aeson
import Data.Bifunctor ( second ) -- base
import Data.Either ( partitionEithers ) -- base
import Data.IORef ( IORef, newIORef, readIORef, writeIORef, atomicModifyIORef', modifyIORef' ) -- base
import qualified Data.Map as M -- containers
import Data.String ( fromString ) -- base
import qualified Data.Text as T -- text
import Data.Tuple ( swap ) -- base
import GHC.Generics ( Generic ) -- ghc
import Network.HTTP.Types.Status ( found302 ) -- http-types
import Network.Wai ( requestHeaderHost, responseBuilder ) -- wai
import Servant ( (:<|>)(..), (:>), QueryParam, Capture, Server, Get, Post, Proxy(..), ReqBody, JSON, Raw, serveDirectoryWebApp ) -- servant-server
import Servant.Server ( Application, serve ) -- servant-server
import System.Timeout ( timeout ) -- base

import AutoInterpreter ( runM, spawnInterpreter )
import Completions ( CompletionContext(..) )
import DatabaseContext ( DatabaseContext(..) )
import Exp ( Pattern, Name(..), Exp(..) )
import Message ( Message(..), Pointer, stripLabel )
import Scheduler ( SessionId, UserId, Event(..), SchedulerFn, SchedulerContext(..),
                   newUserId, canonicalizeEvents, getWorkspace, createInitialWorkspace, labelMessage, createWorkspace )
import Workspace ( Workspace(..), WorkspaceId, VersionId )

data Response = OK | Error T.Text deriving ( Generic )

instance FromJSON Response
instance ToJSON Response

data User = User { userId :: UserId } deriving ( Generic )

instance FromJSON User
instance ToJSON User

type StaticAPI = "static" :> Raw
type CommandAPI = "view" :> ReqBody '[JSON] (User, VersionId, [Message], Pointer) :> Post '[JSON] Response
             :<|> "reply" :> ReqBody '[JSON] (User, VersionId, [Either Message Pointer], Message) :> Post '[JSON] Response
             :<|> "wait" :> ReqBody '[JSON] (User, VersionId, [Either Message Pointer]) :> Post '[JSON] Response
type PointerAPI = "pointer" :> Capture "p" Pointer :> Get '[JSON] (Maybe Message)
type AutoCompleteAPI = "completions" :> Capture "sessionId" SessionId :> Get '[JSON] [Pattern]
type NextAPI = "next" :> ReqBody '[JSON] (User, Maybe SessionId) :> Post '[JSON] (Maybe (Workspace, SessionId))
type JoinAPI = "join" :> QueryParam "userId" UserId :> Get '[JSON] User

type API = CommandAPI :<|> NextAPI :<|> JoinAPI :<|> PointerAPI :<|> AutoCompleteAPI

type OverallAPI = StaticAPI :<|> API :<|> Raw

staticHandler :: Server StaticAPI
staticHandler = serveDirectoryWebApp "static"

autoCompleteHandler :: CompletionContext extra -> Server AutoCompleteAPI
autoCompleteHandler compCtxt sessionId = liftIO $ completionsFor compCtxt sessionId

pointerHandler :: (Pointer -> IO Message) -> Server PointerAPI
pointerHandler deref ptr = liftIO $ do
    print ("Pointer", ptr) -- DELETEME
    Just <$> deref ptr -- TODO: Catch error and return Nothing.

-- TODO: Track users that have joined and only accept requests from previously seen users.
-- This will allow us to trivially ignore requests from users who've been falsely suspected
-- of abandoning a workspace, and will allow a new instance to invalidate any outstanding
-- work of a previous instance.
joinHandler :: (Maybe UserId -> IO User) -> Server JoinAPI
joinHandler makeUser oldUserId = liftIO $ do
    print ("Join", oldUserId) -- DELETEME
    makeUser oldUserId

-- TODO: Cache the user's workspace for a time so refreshing and clicking next doesn't lose the workspace.
-- Then determine a policy for giving up on a user response and writing the workspace back into the channel
-- for someone else.
nextHandler :: (VersionId -> IO Workspace) -> (User -> Maybe SessionId -> IO (Maybe (VersionId, SessionId))) -> Server NextAPI
nextHandler lookupWorkspace nextWorkspace (user@(User userId), mSessionId) = liftIO $ do
    print ("Next", userId, mSessionId) -- DELETEME
    mWorkspaceId <- nextWorkspace user mSessionId
    case mWorkspaceId of
        Just (versionId, sessionId) -> fmap (\ws -> Just (ws, sessionId)) (lookupWorkspace versionId)
        Nothing -> return Nothing

commandHandler :: (UserId -> VersionId -> [Event] -> IO ()) -> Server CommandAPI
commandHandler reply = viewHandler :<|> replyHandler :<|> waitHandler
    where viewHandler (User userId, versionId, msgs, ptr) = liftIO $ do
            print ("View", userId, versionId, msgs, ptr) -- DELETEME
            OK <$ reply userId versionId (map Create msgs ++ [Expand ptr])
          replyHandler (User userId, versionId, msgOrPtrs, msg) = liftIO $ do
            print ("Reply", userId, versionId, msgOrPtrs, msg) -- DELETEME
            OK <$ reply userId versionId (map (either Create Expand) msgOrPtrs ++ [Answer msg])
          waitHandler (User userId, versionId, msgOrPtrs) = liftIO $ do
            print ("Wait", userId, versionId, msgOrPtrs) -- DELETEME
            OK <$ reply userId versionId (map (either Create Expand) msgOrPtrs ++ [Submit])

overallHandler :: CompletionContext extra
               -> (Maybe UserId -> IO User)
               -> (User -> Maybe SessionId -> IO (Maybe (VersionId, SessionId)))
               -> (Pointer -> IO Message)
               -> (VersionId -> IO Workspace)
               -> (UserId -> VersionId -> [Event] -> IO ())
               -> Server OverallAPI
overallHandler compCtxt makeUser nextWorkspace deref lookupWorkspace reply
    = staticHandler
 :<|> (commandHandler reply
 :<|> nextHandler lookupWorkspace nextWorkspace
 :<|> joinHandler makeUser
 :<|> pointerHandler deref
 :<|> autoCompleteHandler compCtxt)
 :<|> return (\req respond -> do
                let !(Just host) = requestHeaderHost req
                respond (responseBuilder found302 [("Location", "https://" <> host <> "/static/index.html")] mempty))

data SessionState = SessionState {
    sessionRequestTChan :: TChan VersionId,
    sessionResponseTMVarsRef :: IORef (M.Map WorkspaceId (TMVar (UserId, Event))),
    sessionDrainingTMVarsRef :: IORef (M.Map WorkspaceId (TMVar (UserId, Event))),
    sessionDoneRef :: IORef Bool }

-- For a web service, we're going to want to separate reading from the channel from giving a response.
-- Basically, when a web request comes in, we'll attempt to read a workspace to display from the channel (using
-- System.Timeout.timeout to handle the case when there are no available workspaces). Some policy will be required for users
-- that walk away without responding. Once we've decided a user isn't going to respond, we can reschedule the workspace
-- to a new user and discard any late response from the old user.
-- Probably use a keep-alive policy.
initServer :: DatabaseContext e -> IO Application
initServer dbCtxt = do
    ctxt <- makeSchedulerContext dbCtxt

    userIdRef <- liftIO $ newIORef (M.empty :: M.Map UserId (M.Map SessionId (Maybe VersionId)))
    sessionMapRef <- newIORef (M.empty :: M.Map SessionId SessionState)
    userSessionMapRef <- newIORef (M.empty :: M.Map UserId SessionId)

    let newSessionState = SessionState <$> newTChanIO <*> newIORef M.empty <*> newIORef M.empty <*> newIORef False

        -- TODO: Allow a single user to *simultaneously* make actions in multiple sessions.
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

        blockOnUser sessionId versionId = liftIO $ do
            (ss, False) <- sessionState sessionId -- Should never be a new SessionState.
            let !responseTMVarsRef = sessionResponseTMVarsRef ss
                !drainingTMVarsRef = sessionDrainingTMVarsRef ss
                !requestTChan = sessionRequestTChan ss
            workspaceId <- workspaceIdOf ctxt versionId -- TODO: This could be handled better.
            mResponseTMVar <- M.lookup workspaceId <$> readIORef drainingTMVarsRef
            case mResponseTMVar of
                Nothing -> do
                    responseTMVar <- newEmptyTMVarIO
                    modifyIORef' responseTMVarsRef (M.insert workspaceId responseTMVar)
                    atomically $ writeTChan requestTChan versionId
                    atomically $ takeTMVar responseTMVar
                Just responseTMVar -> do
                    r@(_, resp) <- atomically $ takeTMVar responseTMVar
                    case resp of
                        Submit -> do modifyIORef' drainingTMVarsRef (M.delete workspaceId); return r
                        Answer _ -> do modifyIORef' drainingTMVarsRef (M.delete workspaceId); return r
                        _ -> return r

        -- TODO: Check if userID is in userIdRef. If not, then ignore.
        replyFromUser userId versionId evts = go =<< canonicalizeEvents ctxt evts
            where go [evt] = do
                    (ss, sessionId) <- userSessionState userId
                    let !responseTMVarsRef = sessionResponseTMVarsRef ss
                    workspaceId <- workspaceIdOf ctxt versionId
                    Just responseTMVar <- atomicModifyIORef' responseTMVarsRef (swap . M.updateLookupWithKey (\_ _ -> Nothing) workspaceId)
                    updateUserSession userId sessionId Nothing
                    atomically $ putTMVar responseTMVar (userId, evt)
                  go (evt:evts) = do
                    (ss, sessionId) <- userSessionState userId
                    workspaceId <- workspaceIdOf ctxt versionId
                    let !responseTMVarsRef = sessionResponseTMVarsRef ss
                        !drainingTMVarsRef = sessionDrainingTMVarsRef ss
                    Just responseTMVar <- atomicModifyIORef' responseTMVarsRef (swap . M.updateLookupWithKey (\_ _ -> Nothing) workspaceId)
                    modifyIORef' drainingTMVarsRef (M.insert workspaceId responseTMVar)
                    updateUserSession userId sessionId Nothing
                    atomically $ putTMVar responseTMVar (userId, evt)
                    mapM_ (atomically . putTMVar responseTMVar . (,) userId) evts -- NOTE: This assumes that a sort of protocol between the interpreter and this and will block forever if it is not met.

        begin = do
            msg <- labelMessage ctxt (Text "What is your question?")
            initWorkspace <- getWorkspace ctxt =<< createInitialWorkspace ctxt msg
            let !q = stripLabel (question initWorkspace)
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
                            !requestTChan = sessionRequestTChan ss
                        autoCtxt <- makeAutoSchedulerContext dbCtxt ctxt sessionId
                        runM (spawnInterpreter (blockOnUser sessionId) (liftIO begin) (liftIO $ writeIORef doneRef True) False autoCtxt)
                        mWorkspaceId <- timeout 10000000 (atomically $ readTChan requestTChan) -- Timeout after 10 seconds.
                        updateUserSession userId sessionId mWorkspaceId
                        return $ fmap (\wsId -> (wsId, sessionId)) mWorkspaceId
                      else do
                        let !doneRef = sessionDoneRef ss
                        isDone <- readIORef doneRef
                        if isDone then do -- TODO: We can clear out the session state for the old session ID now.
                            nextWorkspace user Nothing
                          else do
                            let !requestTChan = sessionRequestTChan ss
                            mWorkspaceId <- timeout 10000000 (atomically $ readTChan requestTChan) -- Timeout after 10 seconds.
                            updateUserSession userId sessionId mWorkspaceId
                            return $ fmap (\wsId -> (wsId, sessionId)) mWorkspaceId

        makeUser Nothing = do
            uId <- newUserId
            atomicModifyIORef' userIdRef $ \uIds -> (M.insert uId M.empty uIds, User uId)
        makeUser (Just oldUserId) = do
            uIds <- readIORef userIdRef
            case M.lookup oldUserId uIds of
                Just _ -> return (User oldUserId)
                Nothing -> makeUser Nothing

        makeSession u@(User userId) mSessionId = do
            sessionId <- newSession ctxt mSessionId
            modifyIORef' userSessionMapRef (M.insert userId sessionId)
            return sessionId

    compCtxt <- makeCompletionContext dbCtxt ctxt

    return $ serve (Proxy :: Proxy OverallAPI) (overallHandler compCtxt makeUser nextWorkspace (dereference ctxt) (getWorkspace ctxt) replyFromUser)
