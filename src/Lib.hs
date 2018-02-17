{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Lib
    ( service
    ) where

import qualified SlackAPI                             as SlackAPI

import           Data.Proxy
import           Servant.API
import           Servant.Server
import           Servant.Utils.StaticFiles            (serveDirectoryFileServer)

import           Network.Wai.Handler.Warp             (Port, runEnv)
import           Network.Wai.Middleware.RequestLogger

type ReleaseBotAPI = SlackAPI.API :<|> StaticAPI

type StaticAPI = "index.html" :> Raw

api :: Proxy ReleaseBotAPI
api = Proxy

service :: IO ()
service = do
  putStrLn "release-bot says hello"
  runEnv defaultPort (logStdout application)

defaultPort :: Port
defaultPort = 8080

application :: Application
application = serve api server

server :: Server ReleaseBotAPI
server = SlackAPI.server :<|> serveDirectoryFileServer "public"
