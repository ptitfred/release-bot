{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Lib
    ( service
    ) where

import qualified SlackAPI                             as SlackAPI

import           Data.Maybe                           (fromMaybe)
import           Data.Proxy
import           Servant.API
import           Servant.Server
import           Servant.Server.StaticFiles           (serveDirectoryFileServer)

import           Network.Wai.Handler.Warp             (Port, runEnv)
import           Network.Wai.Middleware.RequestLogger
import           System.Environment                   (lookupEnv)

type ReleaseBotAPI = SlackAPI.API :<|> StaticAPI

type StaticAPI = Raw
            :<|> "index.html" :> Raw

api :: Proxy ReleaseBotAPI
api = Proxy

service :: IO ()
service = do
  assetsDirectory <- getAssetsDirectory
  port <- getPort
  putStrLn ("release-bot started on tcp:" <> show port)
  runEnv port (logStdout (application assetsDirectory))

defaultPort :: Port
defaultPort = 3000

application :: FilePath -> Application
application = serve api . server

server :: FilePath -> Server ReleaseBotAPI
server assetsDirectory = SlackAPI.server :<|> (assets :<|> assets)
  where
    assets = serveDirectoryFileServer assetsDirectory

getAssetsDirectory :: IO FilePath
getAssetsDirectory = fromMaybe "public" <$> lookupEnv "ASSETS_DIRECTORY"

getPort :: IO Port
getPort = maybe defaultPort read <$> lookupEnv "PORT"
