module Caching.Init ( makeCachingDatabaseContext ) where
import Data.IORef ( newIORef, readIORef, writeIORef ) -- base

import Caching.AutoSchedulerContext ( makeCachingAutoSchedulerContext )
import Caching.CompletionContext ( makeCachingCompletionContext )
import Caching.SchedulerContext ( createCache, makeCachingSchedulerContext )
import DatabaseContext ( DatabaseContext(..) )

-- TODO: Make a dummy DatabaseContext that does nothing leaving the Caching layer as the only source of truth.
makeCachingDatabaseContext :: DatabaseContext e -> IO (DatabaseContext e)
makeCachingDatabaseContext dbCtxt = do
    cacheRef <- newIORef (error "Cache uninitialized")
    return $ DatabaseContext {
                initDB = do
                    initDB dbCtxt
                    writeIORef cacheRef =<< createCache =<< snapshot dbCtxt,
                closeDB = closeDB dbCtxt,
                primitivesToHaskell = primitivesToHaskell dbCtxt,
                snapshot = snapshot dbCtxt,
                makeSchedulerContext = do
                    ctxt <- makeSchedulerContext dbCtxt
                    cache <- readIORef cacheRef
                    ctxt <- makeCachingSchedulerContext cache ctxt
                    return ctxt,
                makeAutoSchedulerContext = \ctxt sessionId -> do
                    autoCtxt <- makeAutoSchedulerContext dbCtxt ctxt sessionId
                    cache <- readIORef cacheRef
                    makeCachingAutoSchedulerContext cache autoCtxt sessionId,
                makeCompletionContext = \ctxt -> do
                    cache <- readIORef cacheRef
                    makeCachingCompletionContext cache ctxt
             }
