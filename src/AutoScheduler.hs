{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module AutoScheduler ( AutoSchedulerContext(..), FunctionId, ProcessId, AddContinuationResult(..), nameToId, idToName ) where
import Data.Int ( Int64 ) -- base
import qualified Data.Map as M -- containers

import Exp ( Pattern, Name(..), Value, Exp', EvalState', Konts', KontsId' )
import Message ( PointerRemapping )
import Scheduler ( SchedulerContext )
import Workspace ( WorkspaceId )

type ProcessId = Int64

type FunctionId = Int64

-- TODO: A better name would be nice.
data AddContinuationResult = NEW | SAME | REPLACED deriving ( Eq, Ord, Show )

data AutoSchedulerContext extra = AutoSchedulerContext {
    thisAnswerId :: FunctionId,
    alternativesFor :: Name -> IO [([Pattern], Exp')],
    allAlternatives :: IO (M.Map Name [([Pattern], Exp')]),
    addCaseFor :: Name -> [Pattern] -> Exp' -> IO (),
    newFunction :: IO Name,
    linkVars :: WorkspaceId -> PointerRemapping -> IO (),
    links :: WorkspaceId -> IO PointerRemapping,
    saveContinuation :: Konts' -> IO (),
    loadContinuation :: KontsId' -> IO Konts',
    recordState :: ProcessId -> EvalState' -> IO (),
    currentState :: ProcessId -> IO EvalState',
    newProcess :: IO ProcessId,
    runQueue :: IO [ProcessId],
    terminate :: ProcessId -> IO (),
    addContinuationArgument :: KontsId' -> Int -> Value -> IO AddContinuationResult,
    continuationArguments :: KontsId' -> IO (Konts', [Value]),
    schedulerContext :: SchedulerContext extra
  }

nameToId :: FunctionId -> Name -> FunctionId
nameToId answerId ANSWER = answerId
nameToId        _ (LOCAL i) = fromIntegral i

idToName :: FunctionId -> FunctionId -> Name
idToName answerId fId | answerId == fId = ANSWER
                      | otherwise = LOCAL (fromIntegral fId)
