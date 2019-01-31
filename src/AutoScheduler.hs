{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module AutoScheduler ( AutoSchedulerContext(..) ) where
import qualified Data.Map as M -- containers

import Scheduler ( SchedulerContext )
import Exp ( Pattern, Name, Exp', Konts', KontsId' )

data AutoSchedulerContext extra = AutoSchedulerContext {
    alternativesFor :: Name -> IO [([Pattern], Exp')],
    allAlternatives :: IO (M.Map Name [([Pattern], Exp')]),
    addCaseFor :: Name -> [Pattern] -> Exp' -> IO (),
    newFunction :: IO Name,
    saveContinuation :: Konts' -> IO (),
    loadContinuation :: KontsId' -> IO Konts',
    schedulerContext :: SchedulerContext extra
  }
