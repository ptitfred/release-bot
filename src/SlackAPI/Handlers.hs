{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module SlackAPI.Handlers
  ( handleEventPayload
  , handleReleaseCommand
  ) where

import           SlackAPI.Types

import           Control.Monad.Error.Class (throwError)
import           Control.Monad.IO.Class    (liftIO)
import           Data.Monoid               ((<>))
import           Data.Text                 (pack)
import           Servant.Server            (Handler, err400)
import           System.Environment        (getEnv)

handleReleaseCommand :: ReleaseCommandPayload -> Handler ReleaseCommandResponse
handleReleaseCommand (ReleaseCommandPayload (Just pn)) = pure . ChannelResponse
  $ "What about a release of *" <> pn <> "*?"
handleReleaseCommand (ReleaseCommandPayload Nothing) = pure . PrivateResponse
  $ "Missing the project name argument"

handleEventPayload :: EventPayload -> Handler EventResponse
handleEventPayload payload = do
  case payload of
    UrlVerification challenge token -> check challenge token
    EventCallback event             -> handleEvent event

check :: Challenge -> Token -> Handler EventResponse
check challenge token = do
  expectedToken <- liftIO getToken
  if token == expectedToken
  then pure Acknowledge{..}
  else badRequest

handleEvent :: Event -> Handler EventResponse
handleEvent (ReactionAddedEvent ReactionAdded{..}) =
  pure NoResponse

badRequest :: Handler a
badRequest = throwError err400

getToken :: IO Token
getToken = Token . pack <$> getEnv "TOKEN"
