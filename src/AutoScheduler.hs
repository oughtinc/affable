{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module AutoScheduler ( AutoSchedulerContext(..) ) where
import qualified Data.Map as M -- containers

import Exp ( Pattern, Name, Value, Exp', EvalState', Konts', KontsId' )
import Scheduler ( SchedulerContext )

data AutoSchedulerContext extra = AutoSchedulerContext {
    alternativesFor :: Name -> IO [([Pattern], Exp')],
    allAlternatives :: IO (M.Map Name [([Pattern], Exp')]),
    addCaseFor :: Name -> [Pattern] -> Exp' -> IO (),
    newFunction :: IO Name,
    saveContinuation :: Konts' -> IO (),
    loadContinuation :: KontsId' -> IO Konts',
    addContinuationArgument :: KontsId' -> Int -> Value -> IO (),
    enqueueStates :: [(KontsId', Int, EvalState')] -> IO (),
    continuationArguments :: KontsId' -> IO (Konts', [Value]),
    schedulerContext :: SchedulerContext extra
  }
