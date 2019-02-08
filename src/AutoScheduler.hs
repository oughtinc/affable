{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module AutoScheduler ( AutoSchedulerContext(..), ProcessId, AddContinuationResult(..) ) where
import Data.Int ( Int64 ) -- base
import qualified Data.Map as M -- containers

import Exp ( Pattern, Name, Value, Exp', EvalState', Konts', KontsId' )
import Scheduler ( SchedulerContext )

type ProcessId = Int64

-- TODO: A better name would be nice.
data AddContinuationResult = NEW | SAME | REPLACED deriving ( Eq, Ord, Show )

data AutoSchedulerContext extra = AutoSchedulerContext {
    alternativesFor :: Name -> IO [([Pattern], Exp')],
    allAlternatives :: IO (M.Map Name [([Pattern], Exp')]),
    addCaseFor :: Name -> [Pattern] -> Exp' -> IO (),
    newFunction :: IO Name,
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
