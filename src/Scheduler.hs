{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Scheduler ( SchedulerContext(..), SchedulerFn, UserId, Event(..), makeSingleUserScheduler, relabelMessage, fullyExpand ) where
import Message ( Message(..), Pointer, PointerEnvironment, PointerRemapping )
import Workspace ( Workspace(identity), WorkspaceId )

data Event
    = Create Message -- ask
    | Answer Message -- reply
    | Expand Pointer -- view
    | Send WorkspaceId Message -- send
    -- | Join -- This is to support more server-y stuff, i.e. a new person joining the computation.
  deriving ( Show )

type UserId = Int

type SchedulerFn = UserId -> Workspace -> Event -> IO (Maybe Workspace)

data SchedulerContext extra = SchedulerContext {
    createInitialWorkspace :: IO WorkspaceId,
    createWorkspace :: Bool -> Workspace -> Message -> Message -> IO WorkspaceId,
    sendAnswer :: Bool -> Workspace -> Message -> IO (),
    sendMessage :: Bool -> Workspace -> WorkspaceId -> Message -> IO (),
    expandPointer :: Workspace -> Pointer -> IO (),
    getWorkspace :: WorkspaceId -> IO Workspace,
    getNextWorkspace :: IO (Maybe WorkspaceId),
    labelMessage :: Message -> IO Message,
    normalize :: Message -> IO Message,
    generalize :: Message -> IO Message,
    instantiate :: PointerEnvironment -> Message -> IO (PointerRemapping, Message),
    dereference :: Pointer -> IO Message,
    extraContent :: extra -- This is to support making schedulers that can (e.g.) access SQLite directly.
  }

relabelMessage :: SchedulerContext extra -> Message -> IO Message
relabelMessage ctxt (LabeledStructured _ ms) = labelMessage ctxt (Structured ms)
relabelMessage ctxt msg = labelMessage ctxt msg

fullyExpand :: SchedulerContext extra -> Message -> IO Message
fullyExpand ctxt (Reference p) = fullyExpand ctxt =<< dereference ctxt p
fullyExpand ctxt (Structured ms) = Structured <$> mapM (fullyExpand ctxt) ms
fullyExpand ctxt (LabeledStructured _ ms) = Structured <$> mapM (fullyExpand ctxt) ms
fullyExpand ctxt m = return m

makeSingleUserScheduler :: SchedulerContext extra -> IO SchedulerFn
makeSingleUserScheduler ctxt = do
    let scheduler user workspace (Create msg) = do -- TODO: Generalize the Message so every pointer is distinct.
            newWorkspaceId <- createWorkspace ctxt True workspace msg msg
            Just <$> getWorkspace ctxt newWorkspaceId

        scheduler user workspace (Answer msg) = do
            sendAnswer ctxt True workspace msg
            mNewWorkspaceId <- getNextWorkspace ctxt
            case mNewWorkspaceId of
                Just newWorkspaceId -> Just <$> getWorkspace ctxt newWorkspaceId
                Nothing -> return Nothing

        scheduler user workspace (Send ws msg) = do
            sendMessage ctxt True workspace ws msg
            Just <$> getWorkspace ctxt (identity workspace)

        scheduler user workspace (Expand ptr) = do
            expandPointer ctxt workspace ptr
            Just <$> getWorkspace ctxt (identity workspace)

    return scheduler