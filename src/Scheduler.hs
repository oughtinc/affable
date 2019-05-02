{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module Scheduler ( SchedulerContext(..), SchedulerFn, UserId, Event(..), SessionId, SyncFunc, AsyncFunc,
                   sessionIdToBuilder, sessionIdFromText, newSessionId, autoUserId, makeSingleUserScheduler, labelMessage, relabelMessage,
                   fullyExpand, newUserId, userIdToBuilder, userIdFromText, normalize, generalize, canonicalizeEvents,
                   workspaceToMessage, renumberEvent, eventMessage ) where
import qualified Data.Map as M -- containers
import Data.Maybe ( mapMaybe ) -- base
import qualified Data.Set as S -- containers
import Data.String ( fromString ) -- base
import qualified Data.Text as T -- text
import Data.Text.Lazy.Builder ( Builder ) -- text
import Data.Traversable ( mapAccumL ) -- base
import Data.UUID ( UUID, nil ) -- uuid
import qualified Data.UUID.V4 as UUID ( nextRandom ) -- uuid
import Text.Megaparsec ( parseMaybe ) -- megaparsec

import Message ( Message(..), Pointer, PointerEnvironment, PointerRemapping,
                 canonicalizeMessage, normalizeMessage, generalizeMessage, boundPointers, stripLabel, renumberMessage' )
import Util ( parseUUID, uuidToBuilder )
import Time ( LogicalTime )
import Workspace ( Workspace(..), VersionId, WorkspaceId )

data Event
    = Create Message -- ask
    | Answer Message -- reply
    | Expand Pointer -- view
    | Send VersionId Message -- send
    | Submit -- wait
    | Init
  deriving ( Eq, Ord, Show )

type UserId = UUID
type SessionId = UUID

autoUserId :: UserId
autoUserId = nil

type SchedulerFn = UserId -> Workspace -> Event -> IO (Maybe Workspace)

type SyncFunc = forall a. IO a -> IO a
type AsyncFunc = IO () -> IO ()

data SchedulerContext extra = SchedulerContext {
    doAtomically :: SyncFunc,
    createInitialWorkspace :: Message -> IO VersionId,
    newSession :: Maybe SessionId -> IO SessionId,
    -- TODO: XXX Have this take a Bool to specify whether it should make a new version or attach to the current one.
    -- Possibly we should just separate out making a new version of a workspace from the other operations.
    newVersion :: VersionId -> IO (VersionId, LogicalTime),
    createWorkspace :: UserId -> VersionId -> Message -> Message -> IO (VersionId, WorkspaceId, LogicalTime),
    sendAnswer :: UserId -> VersionId -> Message -> IO (),
    sendMessage :: UserId -> VersionId -> VersionId -> Message -> IO (),
    expandPointer :: UserId -> VersionId -> Pointer -> IO (),
    nextPointer :: IO Pointer,
    createPointers :: PointerEnvironment -> IO (),
    remapPointers :: PointerRemapping -> IO (),
    pendingQuestions :: VersionId -> IO [VersionId],
    getWorkspace :: VersionId -> IO Workspace,
    workspaceIdOf :: VersionId -> IO WorkspaceId, -- TODO: This can probably be handled in a better way.
    getNextWorkspace :: IO (Maybe VersionId),
    dereference :: Pointer -> IO Message,
    reifyWorkspace :: VersionId -> IO Message,
    extraContent :: extra -- This is to support making schedulers that can (e.g.) access SQLite directly.
  }


normalize :: SchedulerContext extra -> Message -> IO Message
normalize ctxt msg = do
    doAtomically ctxt $ do
        nextPointerId <- nextPointer ctxt
        let (pEnv, normalizedMsg) = normalizeMessage nextPointerId msg
        createPointers ctxt pEnv
        return normalizedMsg

generalize :: SchedulerContext extra -> Message -> IO Message
generalize ctxt msg = do
    doAtomically ctxt $ do
        nextPointerId <- nextPointer ctxt
        let (mapping, generalizedMsg) = generalizeMessage nextPointerId msg
        remapPointers ctxt mapping
        return generalizedMsg

canonicalizeEvents :: SchedulerContext extra -> [Event] -> IO [Event]
canonicalizeEvents ctxt evts = do
    case traverse boundPointers $ mapMaybe eventMessage evts of
        Right envs -> do
            doAtomically ctxt $ do
                let !env = M.unions envs -- TODO: XXX There could be re-use of bound variables, so this needs to be handled in a smarter manner.
                firstPointerId <- nextPointer ctxt
                let !topPointerId = firstPointerId + M.size env - 1
                let !mapping = M.fromList (zip (M.keys env) [firstPointerId ..])
                let !evts' = map (renumberEvent mapping) evts
                let canonicalizeMsg !top msg = (top + M.size pEnv, pEnv)
                        where (pEnv, _) = canonicalizeMessage top msg
                let !pEnv = M.unions $ snd $ mapAccumL canonicalizeMsg (topPointerId + 1) (mapMaybe eventMessage evts')
                createPointers ctxt pEnv
                return evts'
        -- Left p -> return (Left p)
        -- TODO: XXX Either decide to assume evts is well-formed, and enforce it, or propagate the error.

labelMessage :: SchedulerContext extra -> Message -> IO Message
labelMessage ctxt msg@(LabeledStructured _ _) = error $ "labelMessage: "++show msg
labelMessage ctxt msg@(Structured ms) = do
    doAtomically ctxt $ do
        p <- nextPointer ctxt
        createPointers ctxt (M.singleton p msg)
        return $ LabeledStructured p ms
labelMessage ctxt msg = do
    doAtomically ctxt $ do
        p <- nextPointer ctxt
        createPointers ctxt (M.singleton p msg)
        return $ LabeledStructured p [msg]

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
            msg' <- normalize ctxt msg
            (vId, _) <- newVersion ctxt (identity workspace)
            (childVId, _, _) <- createWorkspace ctxt userId vId msg msg'
            Just <$> getWorkspace ctxt childVId

        scheduler userId workspace (Answer msg) = do
            msg <- normalize ctxt msg
            (vId, _) <- newVersion ctxt (identity workspace)
            sendAnswer ctxt userId vId msg
            mNewWorkspaceId <- getNextWorkspace ctxt
            case mNewWorkspaceId of
                Just newWorkspaceId -> Just <$> getWorkspace ctxt newWorkspaceId
                Nothing -> return Nothing

        scheduler userId workspace (Send ws msg) = do
            msg <- normalize ctxt msg
            (vId, _) <- newVersion ctxt (identity workspace)
            sendMessage ctxt userId vId ws msg
            Just <$> getWorkspace ctxt (identity workspace)

        scheduler userId workspace (Expand ptr) = do
            (vId, _) <- newVersion ctxt (identity workspace)
            expandPointer ctxt userId vId ptr
            Just <$> getWorkspace ctxt (identity workspace)

        scheduler userId workspace Submit = return $ Just workspace

        scheduler userId workspace Init = return $ Just workspace

    return scheduler

-- TODO: This could also be done in a way to better reuse existing pointers rather than make new pointers.
-- TODO: Should the expanded pointers be indicated some way?
-- TODO: XXX Modify to show versions.
workspaceToMessage :: M.Map VersionId Workspace -> VersionId -> Message
workspaceToMessage workspaces versionId = go (M.lookup versionId workspaces)
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

sessionIdFromText :: T.Text -> SessionId
sessionIdFromText t = case parseMaybe parseUUID t of Just sessionId -> sessionId

sessionIdToBuilder :: SessionId -> Builder
sessionIdToBuilder = uuidToBuilder

newSessionId :: IO SessionId
newSessionId = UUID.nextRandom

userIdFromText :: T.Text -> UserId
userIdFromText t = case parseMaybe parseUUID t of Just userId -> userId

userIdToBuilder :: UserId -> Builder
userIdToBuilder = uuidToBuilder

newUserId :: IO UserId
newUserId = UUID.nextRandom
