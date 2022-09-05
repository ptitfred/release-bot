{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module SlackAPI.Handlers
  ( handleEventPayload
  , handleReleaseCommand
  ) where

import qualified Github                    (listMergedPullRequestTitlesForRange)
import           Github.Types              (Login (..), PullRequest (..),
                                            PullRequestTitle (..))
import           SlackAPI.Client           (postReleaseMessage)
import           SlackAPI.Types
import           Types

import           Control.Monad             ((>=>))
import           Control.Monad.Error.Class (throwError)
import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Data.Bool                 (bool)
import           Data.List.NonEmpty        (NonEmpty (..), fromList)
import           Data.Text                 (Text, pack)
import           Servant.Server            (Handler, err400)
import           System.Environment        (getEnv)

-- Fake data
isKnownProject :: ProjectName -> Bool
isKnownProject "demo"                     = True
isKnownProject "twitch-analytics-haskell" = True
isKnownProject _                          = False

handleReleaseCommand :: ReleaseCommandPayload -> Handler ReleaseCommandResponse
handleReleaseCommand BadPayload = sayInPrivate "Missing the project name argument"
handleReleaseCommand ReleaseCommandPayload{..} =
  if isKnownProject projectName
  then advertiseRelease channel projectName userId
  else warnUnknownProject projectName

advertiseRelease :: Channel -> ProjectName -> UserId -> Handler ReleaseCommandResponse
advertiseRelease channel projectName userId =
  let interpretSuccess = bool oops yay
      yay = sayInPrivate "Here you go"
      oops = sayInPrivate "Oops! Something went wrong when posting the release message"
      myself = Committer "ptitfred" (Just userId)
      mapping = const myself
      go = postReleaseMessage channel projectName userId
  in listContribs projectName mapping >>= maybe oops (go >=> interpretSuccess)

listContribs :: MonadIO m => ProjectName -> (Text -> Committer) -> m (Maybe (NonEmpty Contrib))
listContribs "demo" mapping = liftIO . pure . pure $
     Contrib (mapping "ptitfred") "Make it sweet and nice" 123 "https://localhost:8080/pull/123"
  :| Contrib (mapping "ptitfred") "Fix bugs"               124 "https://localhost:8080/pull/124"
   : Contrib (mapping "ptitfred") "Moar bugfixes >.<"      125 "https://localhost:8080/pull/125"
   : []
listContribs projectName mapping = liftIO $ do
  either (const Nothing) (prsToContrib (slackId . mapping)) <$> Github.listMergedPullRequestTitlesForRange "ptitfred" projectName "release" "master"

prsToContrib :: (Text -> Maybe UserId) -> [PullRequest] -> Maybe (NonEmpty Contrib)
prsToContrib _       []  = Nothing
prsToContrib mapping prs = Just . fromList . fmap (prToContrib mapping) $ prs

prToContrib :: (Text -> Maybe UserId) -> PullRequest -> Contrib
prToContrib mapping (PullRequest (PullRequestTitle title) _ (Login githubLogin) url number) = Contrib{..}
  where
    committer = Committer{..}
    slackId = mapping githubLogin

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
handleEvent (ReactionAddedEvent _) = pure NoResponse

badRequest :: Handler a
badRequest = throwError err400

getToken :: IO Token
getToken = Token . pack <$> getEnv "TOKEN"
