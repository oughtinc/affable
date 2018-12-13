{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
module Main where
import Data.Foldable ( forM_ ) -- base
import qualified Data.Map as M -- containers
import qualified Data.Text as T -- text
import qualified Data.Text.IO as T -- text
import Database.SQLite.Simple ( Connection, withConnection, execute_, executeMany, query_ ) -- sqlite-simple
import Network.Wai.Handler.Warp ( run ) -- warp
import Servant ( Proxy(..) ) -- servant-server
import Servant.JS ( writeJSForAPI, axios, defAxiosOptions ) -- servant-js
import System.Environment ( getArgs ) -- base

import AutoInterpreter ( makeInterpreterScheduler )
import AutoScheduler ( schedulerContext, allAlternatives )
import CommandLine ( commandLineInteraction )
import Exp ( Exp(..), Name(..), expToHaskell )
import Message ( messageToHaskell, messageToBuilderDB, messageToPattern, parseMessageUnsafe )
import Primitive ( primitives )
import Scheduler ( getWorkspace, createInitialWorkspace, makeSingleUserScheduler )
import SqliteAutoSchedulerContext ( makeSqliteAutoSchedulerContext )
import SqliteSchedulerContext ( makeSqliteSchedulerContext )
import Server ( CommandAPI, overallApp )
import Util ( toText )
import Workspace ( identity )

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["gen-api"] -> writeJSForAPI (Proxy :: Proxy CommandAPI) (axios defAxiosOptions) "static/command-api.js"
        ("serve":args) -> do
            withConnection (fileOrMemory args) $ \conn -> do
                initSqlite conn
                execute_ conn "INSERT OR IGNORE INTO Workspaces (id, logicalTime, parentWorkspaceId, questionAsAsked, questionAsAnswered) \
                              \VALUES (0, 0, NULL, 'What is your question?', 'What is your question?')"
                ctxt <- makeSqliteSchedulerContext conn
                run 8081 (overallApp ctxt)
        ("noauto":args) -> do
            withConnection (fileOrMemory args) $ \conn -> do
                initSqlite conn
                ctxt <- makeSqliteSchedulerContext conn
                initWorkspace <- getWorkspace ctxt =<< createInitialWorkspace ctxt
                scheduler <- makeSingleUserScheduler ctxt
                commandLineInteraction initWorkspace scheduler
        ["export", dbFile, fIdString] -> do
            withConnection dbFile $ \conn -> do
                autoCtxt <- makeSqliteAutoSchedulerContext conn
                alts <- fmap reverse <$> allAlternatives autoCtxt
                let !fId = LOCAL (read fIdString)
                    localLookup ANSWER = maybe [] id $ M.lookup fId alts
                    localLookup f = maybe [] id $ M.lookup f alts
                case M.lookup fId alts of
                    Nothing -> putStrLn ("Function ID " ++ fIdString ++ " not found.")
                    Just root@((topArg,_):_) -> do
                        primitivesToHaskell conn
                        putStrLn "\ndata Message = Text String | Structured [Message]"
                        putStrLn "instance Show Message where\n\
                                 \    showsPrec _ (Text s) = (s++)\n\
                                 \    showsPrec 0 (Structured ms) = foldr (.) id (map (showsPrec 1) ms)\n\
                                 \    showsPrec _ (Structured ms) = ('[':) . foldr (.) id (map (showsPrec 1) ms) . (']':)"
                        -- or, putStrLn "data Message = Text String | Structured [Message] deriving (Show)"
                        putStr "\nmain = print $ "
                        T.putStrLn (toText (expToHaskell localLookup (LetFun ANSWER (Call ANSWER (Value topArg)))))
        _ -> do
            withConnection (fileOrMemory args) $ \conn -> do
                initSqlite conn
                initPrimitives conn
                autoCtxt <- makeSqliteAutoSchedulerContext conn
                let !ctxt = schedulerContext autoCtxt
                initWorkspace <- getWorkspace ctxt =<< createInitialWorkspace ctxt
                scheduler <- makeInterpreterScheduler autoCtxt $! identity initWorkspace
                commandLineInteraction initWorkspace scheduler

fileOrMemory :: [String] -> String
fileOrMemory [file] = file
fileOrMemory _ = ":memory:"

-- TODO: Move this elsewhere.
initPrimitives :: Connection -> IO ()
initPrimitives conn = do
    let prims = map (\(i, p, b, _) -> (i, toText (messageToBuilderDB p), b)) primitives
    executeMany conn "INSERT INTO Primitives (id, pattern, body) VALUES (?, ?, ?)" prims

primitivesToHaskell :: Connection -> IO ()
primitivesToHaskell conn = do
    prims <- query_ conn "SELECT id, pattern, body FROM Primitives" :: IO [(Int, T.Text, T.Text)]
    forM_ prims $ \(i, pattern, body) -> do
        putStr $ "prim" ++ show i ++ " "
        T.putStr (toText (messageToPattern (parseMessageUnsafe pattern)))
        putStr " = "
        T.putStrLn body

-- TODO: Move this elsewhere at some point.
initSqlite :: Connection -> IO ()
initSqlite conn = do
    execute_ conn "\
       \CREATE TABLE IF NOT EXISTS Workspaces (\n\
       \    id INTEGER PRIMARY KEY ASC,\n\
       \    logicalTime INTEGER NOT NULL,\n\
       \    parentWorkspaceId INTEGER NULL,\n\
       \    questionAsAsked TEXT NOT NULL,\n\
       \    questionAsAnswered TEXT NOT NULL,\n\
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
    execute_ conn "\
       \CREATE TABLE IF NOT EXISTS Functions (\n\
       \    id INTEGER PRIMARY KEY ASC,\n\
       \    isAnswer INTEGER NOT NULL DEFAULT 0\n\
       \);"
    execute_ conn "\
       \CREATE TABLE IF NOT EXISTS Alternatives (\n\
       \    function INTEGER NOT NULL,\n\
       \    pattern TEXT NOT NULL,\n\
       \    body TEXT NOT NULL,\n\
       \    FOREIGN KEY ( function ) REFERENCES Functions ( id ) ON DELETE CASCADE\n\
       \    PRIMARY KEY ( function ASC, pattern ASC )\n\
       \);"
    execute_ conn "\
       \CREATE TABLE IF NOT EXISTS Primitives (\n\
       \    id INTEGER PRIMARY KEY ASC,\n\
       \    pattern TEXT NOT NULL,\n\
       \    body TEXT NOT NULL\n\
       \);"
