{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Scheduler ( SchedulerContext(..), SchedulerFn, UserId, Event(..), makeSingleUserScheduler ) where
import Message
import Workspace ( Workspace(identity), WorkspaceId )

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

type SchedulerFn = UserId -> Workspace -> Event -> IO (Maybe Workspace)

-- TODO: This will need to be extended.

data SchedulerContext extra = SchedulerContext {
    createWorkspace :: Workspace -> Message -> IO WorkspaceId,
    sendAnswer :: Workspace -> Message -> IO (),
    sendMessage :: Workspace -> WorkspaceId -> Message -> IO (),
    expandPointer :: Workspace -> Pointer -> IO (),
    getWorkspace :: WorkspaceId -> IO Workspace,
    getNextWorkspace :: IO (Maybe WorkspaceId),
    extraContent :: extra -- This is to support making schedulers that can (e.g.) access SQLite directly.
  }

-- NOTE: This is just a basic start.
-- TODO: Need to normalize messages and save pointers.
makeSingleUserScheduler :: SchedulerContext extra -> IO SchedulerFn
makeSingleUserScheduler ctxt = do
    let scheduler user workspace (Create msg) = do
            newWorkspaceId <- createWorkspace ctxt workspace msg
            Just <$> getWorkspace ctxt newWorkspaceId

        scheduler user workspace (Answer msg) = do
            sendAnswer ctxt workspace msg
            mNewWorkspaceId <- getNextWorkspace ctxt
            case mNewWorkspaceId of
                Just newWorkspaceId -> Just <$> getWorkspace ctxt newWorkspaceId
                Nothing -> return Nothing

        scheduler user workspace (Send ws msg) = do
            sendMessage ctxt workspace ws msg
            Just <$> getWorkspace ctxt (identity workspace)

        scheduler user workspace (Expand ptr) = do
            expandPointer ctxt workspace ptr
            Just <$> getWorkspace ctxt (identity workspace)

    return scheduler
