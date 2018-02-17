{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module SlackAPI
  ( API
  , server
  , Challenge(..)
  ) where

import           SlackAPI.Handlers
import           SlackAPI.Types

import           Servant.API
import           Servant.Server

type API = "slack" :> ReqBody '[JSON] Payload :> Post '[JSON] Response

server :: Server API
server = handlePayload
