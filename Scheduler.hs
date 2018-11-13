{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Scheduler ( SchedulerContext(..), SchedulerFn, UserId, WorkspaceId, Event, makeSingleUserScheduler ) where
import Data.Int ( Int64 ) -- base
import Data.IORef ( newIORef, readIORef, writeIORef ) -- base

import DataModel ( WorkspaceId )
import Message
import Workspace

-- Want to spawn new workspaces.
-- Want to update a current workspace consuming some logical time.
-- Want to send messages to existing workspaces.

data Event
    = Create Message
    | Answer WorkspaceId Message
    | Expand WorkspaceId Pointer -- TODO: Do I want this here?
    | Send WorkspaceId Message
    -- | Join -- This is to support more server-y stuff, i.e. a new person joining the computation.
  deriving ( Show )

type UserId = Int

type SchedulerFn = UserId -> WorkspaceId -> Event -> IO (Maybe Workspace)

-- TODO: This will need to be extended.

data SchedulerContext extra = SchedulerContext {
    createWorkspace :: Maybe WorkspaceId -> Message -> IO (),
    sendAnswer :: WorkspaceId -> Message -> IO (),
    sendMessage :: WorkspaceId -> Message -> IO (),
    expandPointer :: WorkspaceId -> Pointer -> IO (),
    getWorkspace :: WorkspaceId -> IO Workspace,
    getNextWorkspace :: IO (Maybe WorkspaceId),
    extraContent :: extra -- This is to support making schedulers that can (e.g.) access SQLite directly.
  }

-- NOTE: This is just a basic start.
makeSingleUserScheduler :: SchedulerContext extra -> IO SchedulerFn
makeSingleUserScheduler ctxt = do
    currentIdRef <- newIORef Nothing

    let scheduler user workspaceId (Create msg) = do
            currentId <- readIORef currentIdRef
            createWorkspace ctxt currentId msg
            Just <$> getWorkspace ctxt workspaceId

        scheduler user workspaceId (Answer ws msg) = do
            sendAnswer ctxt ws msg
            mNewCurrentId <- getNextWorkspace ctxt
            case mNewCurrentId of
                Just newCurrentId -> do
                    writeIORef currentIdRef (Just newCurrentId)
                    Just <$> getWorkspace ctxt newCurrentId
                Nothing -> return Nothing

        scheduler user workspaceId (Send ws msg) = do
            sendMessage ctxt ws msg
            Just <$> getWorkspace ctxt workspaceId

        scheduler user workspaceId (Expand ws ptr) = do
            expandPointer ctxt ws ptr
            Just <$> getWorkspace ctxt workspaceId

    return scheduler
