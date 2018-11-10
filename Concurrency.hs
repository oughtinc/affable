{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Concurrency where
import Control.Monad.IO.Class ( MonadIO(..) ) -- base
import Control.Monad.Trans.Class ( MonadTrans(..) ) -- transformers
import Control.Monad.Trans.Reader ( ReaderT, runReaderT ) -- transformers
import qualified Control.Monad.Trans.Reader -- transformers
import Control.Monad.Trans.Cont ( ContT(..), callCC ) -- transformers
import Data.IORef ( IORef, newIORef, readIORef, writeIORef, modifyIORef', atomicModifyIORef' ) -- base

-- Want to spawn new workspaces.
-- Want to update a current workspace consuming some logical time.
-- Want to send messages to existing workspaces.


{-
data Event
    = Create Message
    | Answer Workspace Message
    | Send Workspace Message
-}

-- The collection of workspaces and whatever is necessary to support scheduling/caching.
    
-- TODO
data Context = Context -- { workspaces :: IORef (IdMap ChanDescr), runQueue :: IORef (Queue Process) }

newtype ProcessM a = ProcessM { unProcessM :: ContT () (ReaderT Context IO) a }
    deriving ( Functor, Applicative, Monad, MonadIO )

