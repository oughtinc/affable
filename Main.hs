{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE RankNTypes #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
module Main where
import Database.SQLite.Simple ( Connection, withConnection, execute_ ) -- sqlite-simple
import Network.Wai.Handler.Warp ( run ) -- warp
import Servant ( (:<|>)(..), (:>), Server, Get, Post, Proxy(..), Capture, QueryParam, ReqBody, JSON, Raw, serveDirectoryWebApp ) -- servant-server
import Servant.JS ( writeJSForAPI, axios, defAxiosOptions ) -- servant-js
import Servant.Server ( serve, Application ) -- servant-server
import System.Environment ( getArgs ) -- base

import Command
import CommandLine ( commandLineInteraction )
import Message
import Scheduler ( SchedulerContext, makeSingleUserScheduler )
import SqliteSchedulerContext ( makeSqliteSchedulerContext )
import Time
import Workspace

-- Servant - ( TODO: move this elsewhere at some point ) -------------------------------------------------------------------------------------------------------------

-- TODO: Probably specify an generic "response" type and take in a generic "request" type that is basically a string.
-- Do parsing and some degree of rendering to text server-side.
type StaticAPI = "static" :> Raw
type CommandAPI = "command" :> ReqBody '[JSON] Command :> Post '[JSON] Workspace
             :<|> "test" :> Get '[JSON] Command

type OverallAPI = StaticAPI :<|> CommandAPI

staticHandler :: Server StaticAPI
staticHandler = serveDirectoryWebApp "static"

-- TODO: This is just placeholder.
commandHandler :: Connection -> Server CommandAPI
commandHandler conn
    = (\cmd -> return $ emptyWorkspace (Structured [Text "foo ", Structured [Text "bar baz ", Reference 2, Text " ", Location 3]]))
 :<|> return (Reply (Structured [Text "foo ", Structured [Text "bar baz ", Reference 2, Text " ", Location 3]]))

overallHandler :: Connection -> Server OverallAPI
overallHandler conn = staticHandler :<|> commandHandler conn

overallApp :: Connection -> Application
overallApp conn = serve (Proxy :: Proxy OverallAPI) (overallHandler conn)

-- Main --------------------------------------------------------------------------------------------------------------------------------------------------------------

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["gen-api"] -> writeJSForAPI (Proxy :: Proxy CommandAPI) (axios defAxiosOptions) "static/command-api.js"
        ["serve"] -> do
            withConnection ":memory:" $ \conn -> do -- TODO: For now. I do want this to be persistent in the long run.
                initSqlite conn
                run 8081 (overallApp conn)
        _ -> do
            withConnection ":memory:" $ \conn -> do -- TODO: For now. I do want this to be persistent in the long run.
                initSqlite conn
                execute_ conn "INSERT INTO Workspaces (id, logicalTime, parentWorkspaceId, question) VALUES (0, 0, NULL, 'What is your question?')"
                ctxt <- makeSqliteSchedulerContext conn
                scheduler <- makeSingleUserScheduler ctxt
                commandLineInteraction scheduler

-- TODO: Move this elsewhere at some point.
initSqlite :: Connection -> IO ()
initSqlite conn = do
    execute_ conn "\
       \CREATE TABLE IF NOT EXISTS Workspaces (\n\
       \    id INTEGER PRIMARY KEY ASC,\n\
       \    logicalTime INTEGER NOT NULL,\n\
       \    parentWorkspaceId INTEGER NULL,\n\
       \    question TEXT NOT NULL,\n\
       \    FOREIGN KEY ( parentWorkspaceId ) REFERENCES Workspaces ( id ) ON DELETE CASCADE\n\
       \);"
    execute_ conn "\
       \CREATE TABLE IF NOT EXISTS Messages (\n\
       \    id INTEGER PRIMARY KEY ASC,\n\
       \    logicalTimeSent INTEGER NOT NULL,\n\
       \    sourceWorkspaceId INTEGER NOT NULL,\n\
       \    targetWorkspaceId INTEGER NOT NULL,\n\
       \    content TEXT NOT NULL,\n\
       \    FOREIGN KEY ( sourceWorkspaceId ) REFERENCES Workspaces ( id ) ON DELETE CASCADE\n\
       \    FOREIGN KEY ( targetWorkspaceId ) REFERENCES Workspaces ( id ) ON DELETE CASCADE\n\
       \);"
    execute_ conn "\
       \CREATE TABLE IF NOT EXISTS Pointers (\n\
       \    id INTEGER PRIMARY KEY ASC,\n\
       \    content TEXT NOT NULL\n\
       \);"
    execute_ conn "\
       \CREATE TABLE IF NOT EXISTS Answers (\n\
       \    workspaceId INTEGER PRIMARY KEY ASC, -- NOT NULL,\n\
       \    logicalTimeAnswered INTEGER NOT NULL,\n\
       \    answer TEXT NOT NULL,\n\
       \    FOREIGN KEY ( workspaceId ) REFERENCES Workspaces ( id ) ON DELETE CASCADE\n\
       \);"
    execute_ conn "\
       \CREATE TABLE IF NOT EXISTS ExpandedPointers (\n\
       \    workspaceId INTEGER NOT NULL,\n\
       \    pointerId INTEGER NOT NULL,\n\
       \    logicalTimeExpanded INTEGER NOT NULL,\n\
       \    FOREIGN KEY ( workspaceId ) REFERENCES Workspaces ( id ) ON DELETE CASCADE\n\
       \    FOREIGN KEY ( pointerId ) REFERENCES Pointers ( id ) ON DELETE CASCADE\n\
       \    PRIMARY KEY ( workspaceId ASC, pointerId ASC )\n\
       \);"
    execute_ conn "\
       \CREATE TABLE IF NOT EXISTS Commands (\n\
       \    workspaceId INTEGER NOT NULL,\n\
       \    localTime INTEGER NOT NULL,\n\
       \    command TEXT NOT NULL,\n\
       \    FOREIGN KEY ( workspaceId ) REFERENCES Workspaces ( id ) ON DELETE CASCADE\n\
       \    PRIMARY KEY ( workspaceId ASC, localTime ASC )\n\
       \);"
