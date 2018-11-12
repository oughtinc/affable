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
import Database.SQLite.Simple ( Connection, Query, open, close, query, execute, execute_, executeMany, executeNamed, lastInsertRowId ) -- sqlite-simple
import Network.Wai.Handler.Warp ( run ) -- warp
import Servant ( (:<|>)(..), (:>), Server, Get, Post, Proxy(..), Capture, QueryParam, ReqBody, JSON, Raw, serveDirectoryWebApp ) -- servant-server
import Servant.JS ( writeJSForAPI, axios, defAxiosOptions ) -- servant-js
import Servant.Server ( serve, Application ) -- servant-server
import System.Environment ( getArgs ) -- base
import Text.Megaparsec ( parseTest ) -- megaparsec

import Command
import Message
import Time
import Workspace

-- TODO: Probably specify an generic "response" type and take in a generic "request" type that is basically a string.
-- Do parsing and some degree of rendering to text server-side.
type StaticAPI = "static" :> Raw
type CommandAPI = "command" :> ReqBody '[JSON] Command :> Post '[JSON] Workspace
             :<|> "test" :> Get '[JSON] Command

type OverallAPI = StaticAPI :<|> CommandAPI

staticHandler :: Server StaticAPI
staticHandler = serveDirectoryWebApp "static"

commandHandler :: Connection -> Server CommandAPI
commandHandler conn
    = (\cmd -> return $ Workspace {
                            question = Structured [Text "foo ", Structured [Text "bar baz ", Reference 2, Location 3]],
                            time = Time 30 })
 :<|> return (Reply (Structured [Text "foo ", Structured [Text "bar baz ", Reference 2, Location 3]]))

overallHandler :: Connection -> Server OverallAPI
overallHandler conn = staticHandler :<|> commandHandler conn

overallApp :: Connection -> Application
overallApp conn = serve (Proxy :: Proxy OverallAPI) (overallHandler conn)

main :: IO ()
main = server

server :: IO ()
server = do
    args <- getArgs
    case args of
        ["gen"] -> writeJSForAPI (Proxy :: Proxy CommandAPI) (axios defAxiosOptions) "static/command-api.js"
        _ -> do
            conn <- open ":memory:" -- TODO: For now. I do want this to be persistent in the long run.
            run 8081 (overallApp conn)
            close conn -- TODO: Doesn't get here. Use approach in https://stackoverflow.com/a/45846292

-- example = parseTest messageParser "foo [bar baz [quux]] $2 @4"
example = parseTest commandParser "send @81 foo [bar baz [quux]] $2 @4"
