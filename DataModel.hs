{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module DataModel where
import Data.Int ( Int64 ) -- base
import Data.Text ( Text ) -- text
import Database.SQLite.Simple ( ToRow(..) ) -- sqlite-simple
import Database.SQLite.Simple.FromRow ( FromRow(..), field ) -- sqlite-simple

-- TODO: Logical times maybe should be part of primary key. They also are structured objects so maybe a foreign key to something... (?)

-- NOTE: This is clearly tied to SQLite, though it may well be nice to use a server-based system
-- in the future, e.g. Postgres. The changes to support or generalize this should be pretty trivial
-- so I'm not too concerned about it right now.

type WorkspaceId = Int64

type LogicalTime = Int64

{-
CREATE TABLE IF NOT EXISTS Workspaces (
    id INTEGER PRIMARY KEY ASC,
    logicalTime INTEGER NOT NULL,
    parentWorkspaceId INTEGER NULL,
    question TEXT NOT NULL,
    FOREIGN KEY ( parentWorkspaceId ) REFERENCES Workspaces ( id ) ON DELETE CASCADE
);
-}

data WorkspaceRow = WorkspaceRow {
    workspaceRow_id :: WorkspaceId,
    logicalTime :: LogicalTime,
    parentWorkspaceId :: Maybe WorkspaceId,
    question :: Text }
  deriving ( Show )

instance FromRow WorkspaceRow where
    fromRow = WorkspaceRow <$> field <*> field <*> field <*> field

instance ToRow WorkspaceRow where
    toRow r = toRow (workspaceRow_id r, logicalTime r, parentWorkspaceId r, question r)

{-
CREATE TABLE IF NOT EXISTS Messages (
    id INTEGER PRIMARY KEY ASC,
    logicalTimeSent INTEGER NOT NULL,
    sourceWorkspaceId INTEGER NOT NULL,
    targetWorkspaceId INTEGER NOT NULL,
    content TEXT NOT NULL,
    FOREIGN KEY ( sourceWorkspaceId ) REFERENCES Workspaces ( id ) ON DELETE CASCADE
    FOREIGN KEY ( targetWorkspaceId ) REFERENCES Workspaces ( id ) ON DELETE CASCADE
);
-}

data MessageRow = MessageRow {
    messageRow_id :: Int64,
    logicalTimeSent :: LogicalTime,
    sourceWorkspaceId :: WorkspaceId,
    targetWorkspaceId :: WorkspaceId,
    messageRow_content :: Text }
  deriving ( Show )

instance FromRow MessageRow where
    fromRow = MessageRow <$> field <*> field <*> field <*> field <*> field

instance ToRow MessageRow where
    toRow r = toRow (messageRow_id r, logicalTimeSent r, sourceWorkspaceId r, targetWorkspaceId r, messageRow_content r)

{-
CREATE TABLE IF NOT EXISTS Pointers (
    id INTEGER PRIMARY KEY ASC,
    content TEXT NOT NULL
);
-}

data PointerRow = PointerRow {
    pointerRow_id :: Int64,
    pointerRow_content :: Text }
  deriving ( Show )

instance FromRow PointerRow where
    fromRow = PointerRow <$> field <*> field

instance ToRow PointerRow where
    toRow r = toRow (pointerRow_id r, pointerRow_content r)

{-
CREATE TABLE IF NOT EXISTS Answers (
    workspaceId INTEGER PRIMARY KEY ASC, -- NOT NULL,
    logicalTimeAnswered INTEGER NOT NULL,
    answer TEXT NOT NULL,
    FOREIGN KEY ( workspaceId ) REFERENCES Workspaces ( id ) ON DELETE CASCADE
);
-}

data AnswerRow = AnswerRow {
    answerRow_workspaceId :: WorkspaceId,
    logicalTimeAnswered :: LogicalTime,
    answer :: Text }
  deriving ( Show )

instance FromRow AnswerRow where
    fromRow = AnswerRow <$> field <*> field <*> field

instance ToRow AnswerRow where
    toRow r = toRow (answerRow_workspaceId r, logicalTimeAnswered r, answer r)

{-
CREATE TABLE IF NOT EXISTS ExpandedPointers (
    workspaceId INTEGER NOT NULL,
    pointerId INTEGER NOT NULL,
    logicalTimeExpanded INTEGER NOT NULL,
    FOREIGN KEY ( workspaceId ) REFERENCES Workspaces ( id ) ON DELETE CASCADE
    FOREIGN KEY ( pointerId ) REFERENCES Pointers ( id ) ON DELETE CASCADE
    PRIMARY KEY ( workspaceId ASC, pointerId ASC )
);
-}

data ExpandedPointerRow = ExpandedPointerRow {
    expandedPointerRow_workspaceId :: WorkspaceId,
    pointerId :: Int64,
    logicalTimeExpanded :: LogicalTime }
  deriving ( Show )

instance FromRow ExpandedPointerRow where
    fromRow = ExpandedPointerRow <$> field <*> field <*> field

instance ToRow ExpandedPointerRow where
    toRow r = toRow (expandedPointerRow_workspaceId r, pointerId r, logicalTimeExpanded r)

{-
CREATE TABLE IF NOT EXISTS Commands (
    workspaceId INTEGER NOT NULL,
    localTime INTEGER NOT NULL,
    command TEXT NOT NULL,
    FOREIGN KEY ( workspaceId ) REFERENCES Workspaces ( id ) ON DELETE CASCADE
    PRIMARY KEY ( workspaceId ASC, localTime ASC )
);
-}

data CommandRow = CommandRow {
    commandRow_workspaceId :: WorkspaceId,
    localTime :: Int64,
    command :: Text }
  deriving ( Show )

instance FromRow CommandRow where
    fromRow = CommandRow <$> field <*> field <*> field

instance ToRow CommandRow where
    toRow r = toRow (commandRow_workspaceId r, localTime r, command r)

-- RunQueue ?
