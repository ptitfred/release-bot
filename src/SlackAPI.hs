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

type API = "slack" :> (
       "events" :> EventsEndpoint
  :<|> "commands" :> "release" :> ReleaseEndpoint
  )

type EventsEndpoint = ReqBody '[JSON] EventPayload :> Post '[JSON] EventResponse

type ReleaseEndpoint = ReqBody '[FormUrlEncoded] ReleaseCommandPayload :> Post '[JSON] ReleaseCommandResponse

server :: Server API
server = handleEventPayload
    :<|> handleReleaseCommand
