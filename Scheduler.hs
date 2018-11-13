{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Scheduler where
import Database.SQLite.Simple ( Connection ) -- sqlite-simple

import Message
import Workspace

-- Want to spawn new workspaces.
-- Want to update a current workspace consuming some logical time.
-- Want to send messages to existing workspaces.

data Event
    = Create Message
    | Answer Workspace Message
    | Expand Pointer -- TODO: Do I want this here?
    | Send Workspace Message
    -- | Join -- This is to support more server-y stuff, i.e. a new person joining the computation.
  deriving ( Show )

type UserId = () -- TODO

type SchedulerFn = UserId -> Workspace -> Event -> IO ()

-- TODO: Abstract from the SQLite details.
type SQLiteContext = Connection

makeSQLiteBasedScheduler :: SQLiteContext -> IO SchedulerFn
makeSQLiteBasedScheduler ctxt = return body
    where body user workspace event = return ()
