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
-- TODO: There's an "extra" version that corresponds to replying. Since answers aren't part of workspaces but
-- rather their parents, it would be good to either skip that extra version or include answers for the workspace's
-- question as part of the reflected structure.
workspaceToMessage :: M.Map VersionId Workspace -> VersionId -> Message
workspaceToMessage workspaces versionId = go (M.lookup versionId workspaces)
    where go (Just workspace) | null subQs && null msgs = Structured (Text "Question: ":question workspace:ptrs++prevs)
                              | null subQs = Structured (Text "Question: "
                                                        : question workspace
                                                        : Text " Messages: 1. "
                                                        : (msgs ++ ptrs ++ prevs))
                              | null msgs = Structured (Text "Question: "
                                                       : question workspace
                                                       : Text " Subquestions: 1. "
                                                       : (subQs ++ ptrs ++ prevs))
                              | otherwise =  Structured (Text "Question: "
                                                        : question workspace
                                                        : Text " Messages: 1. "
                                                        : msgs
                                                        ++ (Text " Subquestions: 1. "
                                                           : (subQs ++ ptrs ++ prevs)))
            where prevs = maybe [] (\vId -> [Text " Previous: ", workspaceToMessage workspaces vId]) $ previousVersion workspace
                  ptrs | M.null (expandedPointers workspace) = []
                       | otherwise = [Text " Expanded Pointers: ", Structured (map Reference (M.keys (expandedPointers workspace)))]
                       -- TODO: Should probably intersperse commas or something here --^
                  subQs = goSub 1 (subQuestions workspace)
                  msgs = goMsg 1 (messageHistory workspace)
                  subQVersion wsId ma = do -- Clean this up.
                    w <- M.lookup wsId workspaces
                    case ma of
                        Nothing -> return w
                        _ -> maybe (return w) Just (do wsId' <- previousVersion w; M.lookup wsId' workspaces)
                          -- ^-- This skips the latest version of the subquestion which corresponds to the version where an answer
                          --     was attached and thus looks identical to its previous version. Primitives are an exception.
                  goSub !i [] = []
                  goSub 1 ((wsId, _, ma):qs) -- To avoid [Text "...", Text "..."]
                    = go (subQVersion wsId ma):maybe (goSub 2 qs) (\a -> Text " Answer: ":a:goSub 2 qs) ma
                  goSub i ((wsId, _, ma):qs)
                    = Text (fromString (' ':show i ++ ". ")):go (subQVersion wsId ma):maybe (goSub (i+1) qs) (\a -> Text " Answer: ":a:goSub (i+1) qs) ma
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
