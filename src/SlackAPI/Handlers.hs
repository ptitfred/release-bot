{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module SlackAPI.Handlers
  ( handleEventPayload
  , handleReleaseCommand
  ) where

import           SlackAPI.Client           (postReleaseMessage)
import           SlackAPI.Types
import           Types

import           Control.Monad.Error.Class (throwError)
import           Control.Monad.IO.Class    (liftIO)
import           Data.List.NonEmpty        (NonEmpty (..))
import           Data.Text                 (Text, pack)
import           Servant.Server            (Handler, err400)
import           System.Environment        (getEnv)

-- Fake data
isKnownProject :: ProjectName -> Bool
isKnownProject "pouet"       = True
isKnownProject "release-bot" = True
isKnownProject _             = False

handleReleaseCommand :: ReleaseCommandPayload -> Handler ReleaseCommandResponse
handleReleaseCommand BadPayload = sayInPrivate "Missing the project name argument"
handleReleaseCommand ReleaseCommandPayload{..} =
  if isKnownProject projectName
  then advertiseRelease channel projectName userId
  else warnUnknownProject projectName

advertiseRelease :: Channel -> ProjectName -> UserId -> Handler ReleaseCommandResponse
advertiseRelease channel projectName userId =
  let interpretSuccess True  = "Here you go"
      interpretSuccess False = "Oops! Something went wrong when posting the release message"
      myself = Committer "ptitfred" userId
      contribs = Contrib myself "Make it sweet and nice" 123 "https://localhost:8080/pull/123"
              :| Contrib myself "Fix bugs"               124 "https://localhost:8080/pull/124"
              :  Contrib myself "Moar bugfixes >.<"      125 "https://localhost:8080/pull/125"
              :  []
      go = postReleaseMessage channel projectName userId contribs
  in go >>= sayInPrivate . interpretSuccess

warnUnknownProject :: ProjectName -> Handler ReleaseCommandResponse
warnUnknownProject pn = sayInPrivate $
  mconcat ["This project ", formatProjectName pn, " is unknown. Can you check?"]

formatProjectName :: ProjectName -> Text
formatProjectName (ProjectName pn) = mconcat ["*", pn, "*"]

sayInPrivate :: Applicative f => Text -> f ReleaseCommandResponse
sayInPrivate = pure . PrivateResponse

handleEventPayload :: EventPayload -> Handler EventResponse
handleEventPayload (UrlVerification challenge token) = check challenge token
handleEventPayload (EventCallback   event          ) = handleEvent event

check :: Challenge -> Token -> Handler EventResponse
check challenge token = do
  expectedToken <- liftIO getToken
  if token == expectedToken
  then pure Acknowledge{..}
  else badRequest

handleEvent :: Event -> Handler EventResponse
handleEvent (ReactionAddedEvent ReactionAdded{..}) = pure NoResponse

badRequest :: Handler a
badRequest = throwError err400

getToken :: IO Token
getToken = Token . pack <$> getEnv "TOKEN"
