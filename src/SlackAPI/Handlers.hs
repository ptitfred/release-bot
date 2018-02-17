{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module SlackAPI.Handlers
  ( handlePayload
  ) where

import           SlackAPI.Types

import           Control.Monad.Error.Class (throwError)
import           Control.Monad.IO.Class    (liftIO)
import           Data.Text                 (pack)
import           Servant.Server            (Handler, err400)
import           System.Environment        (getEnv)

handlePayload :: Payload -> Handler Response
handlePayload payload = do
  case payload of
    UrlVerification challenge token -> check challenge token
    EventCallback event -> handleEvent event

check :: Challenge -> Token -> Handler Response
check challenge token = do
  expectedToken <- liftIO getToken
  if token == expectedToken
  then pure Acknowledge{..}
  else badRequest

handleEvent :: Event -> Handler Response
handleEvent (ReactionAddedEvent ReactionAdded{..}) =
  pure NoResponse

badRequest :: Handler a
badRequest = throwError err400

getToken :: IO Token
getToken = Token . pack <$> getEnv "TOKEN"
