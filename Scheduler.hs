{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Scheduler ( SchedulerContext(..), SchedulerFn, UserId, Event(..), makeSingleUserScheduler ) where
import Message
import Workspace ( Workspace, WorkspaceId )

-- Want to spawn new workspaces.
-- Want to update a current workspace consuming some logical time.
-- Want to send messages to existing workspaces.

data Event
    = Create Message
    | Answer Message
    | Expand Pointer -- TODO: Do I want this here?
    | Send WorkspaceId Message
    -- | Join -- This is to support more server-y stuff, i.e. a new person joining the computation.
  deriving ( Show )

type UserId = Int

type SchedulerFn = UserId -> WorkspaceId -> Event -> IO (Maybe Workspace)

-- TODO: This will need to be extended.

data SchedulerContext extra = SchedulerContext {
    createWorkspace :: WorkspaceId -> Message -> IO WorkspaceId,
    sendAnswer :: WorkspaceId -> Message -> IO (),
    sendMessage :: WorkspaceId -> WorkspaceId -> Message -> IO (),
    expandPointer :: WorkspaceId -> Pointer -> IO (),
    getWorkspace :: WorkspaceId -> IO Workspace,
    getNextWorkspace :: IO (Maybe WorkspaceId),
    extraContent :: extra -- This is to support making schedulers that can (e.g.) access SQLite directly.
  }

-- NOTE: This is just a basic start.
-- TODO: Need to normalize messages and save pointers.
makeSingleUserScheduler :: SchedulerContext extra -> IO SchedulerFn
makeSingleUserScheduler ctxt = do
    let scheduler user workspaceId (Create msg) = do
            newWorkspaceId <- createWorkspace ctxt workspaceId msg
            Just <$> getWorkspace ctxt newWorkspaceId

        scheduler user workspaceId (Answer msg) = do
            sendAnswer ctxt workspaceId msg
            mNewWorkspaceId <- getNextWorkspace ctxt
            case mNewWorkspaceId of
                Just newWorkspaceId -> Just <$> getWorkspace ctxt newWorkspaceId
                Nothing -> return Nothing

        scheduler user workspaceId (Send ws msg) = do
            sendMessage ctxt workspaceId ws msg
            Just <$> getWorkspace ctxt workspaceId

        scheduler user workspaceId (Expand ptr) = do
            expandPointer ctxt workspaceId ptr
            Just <$> getWorkspace ctxt workspaceId

    return scheduler
