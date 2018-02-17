{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module SlackAPI.Handlers
  ( handleEventPayload
  , handleReleaseCommand
  ) where

import           SlackAPI.Client           (postReleaseMessage)
import           SlackAPI.Types

import           Control.Monad             (void)
import           Control.Monad.Error.Class (throwError)
import           Control.Monad.IO.Class    (liftIO)
import           Data.Monoid               ((<>))
import           Data.Text                 (Text, pack)
import           Servant.Server            (Handler, err400)
import           System.Environment        (getEnv)

-- Fake data
isKnownProject :: Text -> Bool
isKnownProject "pouet"       = True
isKnownProject "release-bot" = True
isKnownProject _             = False

handleReleaseCommand :: ReleaseCommandPayload -> Handler ReleaseCommandResponse
handleReleaseCommand (ReleaseCommandPayload (Just pn) userId channel) =
  if isKnownProject pn
  then do
    void $ liftIO (postReleaseMessage channel pn userId)
    sayInPrivate "Here you go"
  else sayInPrivate $ warnUnknownProject pn
handleReleaseCommand (ReleaseCommandPayload Nothing _ _) =
  sayInPrivate "Missing the project name argument"

warnUnknownProject :: Text -> Text
warnUnknownProject pn = "This project " <> formatProjectName pn <> " is unknown. Can you check?"

formatProjectName :: Text -> Text
formatProjectName pn = mconcat ["*", pn, "*"]

sayInPrivate :: Applicative f => Text -> f ReleaseCommandResponse
sayInPrivate = pure . PrivateResponse

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
