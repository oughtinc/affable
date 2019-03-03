{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Scheduler ( SchedulerContext(..), SchedulerFn, UserId, Event(..), SessionId,
                   autoUserId, firstUserId, makeSingleUserScheduler, relabelMessage, fullyExpand,
                   workspaceToMessage, renumberEvent, eventMessage ) where
import Data.Int ( Int64 ) -- base
import qualified Data.Map as M -- containers
import qualified Data.Set as S -- containers
import Data.String ( fromString ) -- base

import Message ( Message(..), Pointer, PointerEnvironment, PointerRemapping, stripLabel, renumberMessage' )
import Workspace ( Workspace(..), WorkspaceId )

data Event
    = Create Message -- ask
    | Answer Message -- reply
    | Expand Pointer -- view
    | Send WorkspaceId Message -- send
    | Submit -- wait
    | Init
  deriving ( Eq, Ord, Show )

type UserId = Int64
type SessionId = Int64

autoUserId :: UserId
autoUserId = 0

firstUserId :: UserId
firstUserId = 1

type SchedulerFn = UserId -> Workspace -> Event -> IO (Maybe Workspace)

data SchedulerContext extra = SchedulerContext {
    createInitialWorkspace :: IO WorkspaceId,
    newSession :: Maybe SessionId -> IO SessionId,
    createWorkspace :: Bool -> UserId -> WorkspaceId -> Message -> Message -> IO WorkspaceId,
    sendAnswer :: Bool -> UserId -> WorkspaceId -> Message -> IO (),
    sendMessage :: Bool -> UserId -> WorkspaceId -> WorkspaceId -> Message -> IO (),
    expandPointer :: UserId -> WorkspaceId -> Pointer -> IO (),
    pendingQuestions :: WorkspaceId -> IO [WorkspaceId],
    getWorkspace :: WorkspaceId -> IO Workspace,
    allWorkspaces :: IO (M.Map WorkspaceId Workspace),
    getNextWorkspace :: IO (Maybe WorkspaceId),
    labelMessage :: Message -> IO Message,
    normalize :: Message -> IO Message,
    canonicalizeEvents :: [Event] -> IO [Event],
    generalize :: Message -> IO Message,
    dereference :: Pointer -> IO Message,
    reifyWorkspace :: WorkspaceId -> IO Message,
    extraContent :: extra -- This is to support making schedulers that can (e.g.) access SQLite directly.
  }

relabelMessage :: SchedulerContext extra -> Message -> IO Message
relabelMessage ctxt = labelMessage ctxt . stripLabel

fullyExpand :: SchedulerContext extra -> Message -> IO Message
fullyExpand ctxt m = go S.empty m -- TODO: Maybe maintain seen Messages to avoid issues with aliases.
    where go !seen (Reference p) = go seen =<< dereference ctxt p
          go !seen (Structured ms) = Structured <$> mapM (go seen) ms
          go !seen (LabeledStructured p ms) | p `S.member` seen = return $ Reference p
                                            | otherwise = Structured <$> mapM (go (S.insert p seen)) ms
          go !seen m = return m

makeSingleUserScheduler :: SchedulerContext extra -> IO SchedulerFn
makeSingleUserScheduler ctxt = do
    let scheduler userId workspace (Create msg) = do
            newWorkspaceId <- createWorkspace ctxt True userId (identity workspace) msg msg
            Just <$> getWorkspace ctxt newWorkspaceId

        scheduler userId workspace (Answer msg) = do
            sendAnswer ctxt True userId (identity workspace) msg
            mNewWorkspaceId <- getNextWorkspace ctxt
            case mNewWorkspaceId of
                Just newWorkspaceId -> Just <$> getWorkspace ctxt newWorkspaceId
                Nothing -> return Nothing

        scheduler userId workspace (Send ws msg) = do
            sendMessage ctxt True userId (identity workspace) ws msg
            Just <$> getWorkspace ctxt (identity workspace)

        scheduler userId workspace (Expand ptr) = do
            expandPointer ctxt userId (identity workspace) ptr
            Just <$> getWorkspace ctxt (identity workspace)

        scheduler userId workspace Submit = return $ Just workspace

        scheduler userId workspace Init = return $ Just workspace

    return scheduler

-- TODO: This could also be done in a way to better reuse existing pointers rather than make new pointers.
-- TODO: Should the expanded pointers be indicated some way?
workspaceToMessage :: M.Map WorkspaceId Workspace -> WorkspaceId -> Message
workspaceToMessage workspaces workspaceId = go (M.lookup workspaceId workspaces)
    where go (Just workspace) | null subQs && null msgs = Structured [Text "Question: ", question workspace]
                              | null subQs = Structured (Text "Question: "
                                                        : question workspace
                                                        : Text " Messages: 1. "
                                                        : msgs)
                              | null msgs = Structured (Text "Question: "
                                                       : question workspace
                                                       : Text " Subquestions: 1. "
                                                       : subQs)
                              | otherwise =  Structured (Text "Question: "
                                                        : question workspace
                                                        : Text " Messages: 1. "
                                                        : msgs
                                                        ++ (Text " Subquestions: 1. "
                                                           : subQs))
            where subQs = goSub 1 (subQuestions workspace)
                  msgs = goMsg 1 (messageHistory workspace)
                  goSub !i [] = []
                  goSub i ((_, _, Nothing):qs) = goSub i qs
                  goSub 1 ((wsId, _, Just a):qs) -- To avoid [Text "...", Text "..."]
                    = go (M.lookup wsId workspaces):Text " Answer:":a:goSub 2 qs
                  goSub i ((wsId, _, Just a):qs)
                    = Text (fromString (' ':show i ++ ". ")):go (M.lookup wsId workspaces):Text " Answer:":a:goSub (i+1) qs
                  goMsg !i [] = []
                  goMsg 1 (m:ms) = m:goMsg 2 ms
                  goMsg i (m:ms) = Text (fromString (' ':show i ++ ". ")):m:goMsg (i+1) ms

renumberEvent :: PointerRemapping -> Event -> Event
renumberEvent mapping (Create m) = Create (renumberMessage' mapping m)
renumberEvent mapping (Answer m) = Answer (renumberMessage' mapping m)
renumberEvent mapping (Send loc m) = Send loc (renumberMessage' mapping m)
renumberEvent mapping m = m

eventMessage :: Event -> Maybe Message
eventMessage (Create m) = Just m
eventMessage (Answer m) = Just m
eventMessage (Send _ m) = Just m
eventMessage _ = Nothing
