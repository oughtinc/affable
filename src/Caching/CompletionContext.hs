{-# LANGUAGE BangPatterns #-}
module Caching.CompletionContext ( makeCachingCompletionContext ) where
import Control.Concurrent.STM ( readTVarIO ) -- stm
import qualified Data.Map as M -- containers

import Caching.SchedulerContext ( CacheState(..) )
import Completions ( CompletionContext(..) )
import Exp ( Pattern )
import Primitive ( primitives )
import Scheduler ( SchedulerContext(..), SessionId )

makeCachingCompletionContext :: CacheState -> SchedulerContext e -> IO (CompletionContext e)
makeCachingCompletionContext cache ctxt = do
    let primPatterns = map (\(_, pattern, _, _) -> pattern) primitives

    return $ CompletionContext {
                    completionsFor = completionsForCaching cache primPatterns,
                    schedulerContext = ctxt
                }

completionsForCaching :: CacheState -> [Pattern] -> SessionId -> IO [Pattern]
completionsForCaching cache primPatterns sessionId = do
    answerId <- (M.! sessionId) <$> readTVarIO (answerFunctionsC cache)
    alts <- (M.! answerId) <$> readTVarIO (alternativesC cache)
    return $ primPatterns ++ foldMap fst alts
