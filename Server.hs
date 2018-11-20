{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Server where
import Servant ( (:<|>)(..), (:>), Server, Get, Post, Proxy(..), Capture, QueryParam, ReqBody, JSON, Raw, serveDirectoryWebApp ) -- servant-server
import Servant.Server ( serve, Application ) -- servant-server

import Command ( Command(..) )
import Workspace ( Workspace, emptyWorkspace )
import Message ( Message(..) )
import Scheduler ( SchedulerContext )

-- TODO: Probably specify an generic "response" type and take in a generic "request" type that is basically a string.
-- Do parsing and some degree of rendering to text server-side.
type StaticAPI = "static" :> Raw
type CommandAPI = "command" :> ReqBody '[JSON] Command :> Post '[JSON] Workspace
             :<|> "test" :> Get '[JSON] Command

type OverallAPI = StaticAPI :<|> CommandAPI

staticHandler :: Server StaticAPI
staticHandler = serveDirectoryWebApp "static"

-- TODO: This is just placeholder.
commandHandler :: SchedulerContext e -> Server CommandAPI
commandHandler ctxt
    = (\cmd -> return $ emptyWorkspace (Structured [Text "foo ", Structured [Text "bar baz ", Reference 2, Text " ", Location 3]]))
 :<|> return (Reply (Structured [Text "foo ", Structured [Text "bar baz ", Reference 2, Text " ", Location 3]]))

overallHandler :: SchedulerContext e -> Server OverallAPI
overallHandler ctxt = staticHandler :<|> commandHandler ctxt

overallApp :: SchedulerContext e -> Application
overallApp ctxt = serve (Proxy :: Proxy OverallAPI) (overallHandler ctxt)
