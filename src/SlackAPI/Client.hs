{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeOperators              #-}

module SlackAPI.Client
  ( lookupMessage
  , postMessage
  , addReaction
  , postReleaseMessage
  , Message(..)
  ) where

import           SlackAPI.Types

import           Data.Aeson              hiding (Result)
import           Data.Aeson.Types        (typeMismatch)
import           Data.Either             (isRight)
import           Data.Monoid             ((<>))
import           Data.Proxy
import           Data.Text               (Text, pack)
import           Network.HTTP.Client.TLS (newTlsManager)
import           Servant.API
import           Servant.Client          (BaseUrl (..), ClientEnv (..), ClientM,
                                          Scheme (Https), client, runClientM)
import           System.Environment      (getEnv)

postReleaseMessage :: Channel -> IO Bool
postReleaseMessage c = do
  message <- postMessage c "Reaction with :+1: on this message to approve release."
  case message of
    Just (_, ts) -> addReaction c ts "+1"
    Nothing      -> pure False

lookupMessage :: Channel -> Timestamp -> IO (Maybe Message)
lookupMessage channel ts =
  interpretResult <$> lookupMessageEither channel ts
  where
    interpretResult = either (const Nothing) safeHead

postMessage :: Channel -> Text -> IO (Maybe (Channel, Timestamp))
postMessage channel text = do
  token <- getAccessToken
  let request = ChatPostMessageRequest{..}
  interpretResult <$> runSlackClient (postMessageClient (Just ("Bearer " <> token)) request)
  where
    interpretResult = either (const Nothing) (Just . extractIds)

addReaction :: Channel -> Timestamp -> Text -> IO Bool
addReaction channel ts reaction = do
  token <- getAccessToken
  let request = ReactionsAddRequest{..}
  isRight <$> runSlackClient (addReactionClient (Just ("Bearer " <> token)) request)

extractIds :: ChatPostMessage -> (Channel, Timestamp)
extractIds ChatPostMessage{..} = (channel, ts)

lookupMessageEither :: Channel -> Timestamp -> IO (Either String [Message])
lookupMessageEither channel ts = do
  token <- getAccessToken
  fmap getMessages <$> runSlackClient (action token)
  where
    action t = lookupMessageClient (Just t) (Just channel) (Just ts) Nothing (Just 1) (Just True)

runSlackClient :: ClientM (Result a) -> IO (Either String a)
runSlackClient c = either (Left . show) getResult <$> (getClientEnv >>= runClientM c)

getAccessToken :: IO Token
getAccessToken = Token . pack <$> getEnv "BOT_ACCESS_TOKEN"

getClientEnv :: IO ClientEnv
getClientEnv = do
  manager <- newTlsManager
  let baseUrl = BaseUrl { baseUrlScheme = Https
                        , baseUrlHost = "slack.com"
                        , baseUrlPort = 443
                        , baseUrlPath = "/"
                        }
  pure $ ClientEnv manager baseUrl

data Message = Message { text :: Text } deriving Show

instance FromJSON Message where
  parseJSON (Object o) = Message <$> o .: "text"
  parseJSON invalid    = typeMismatch "Message" invalid

type API = "api" :> (ChannelsHistoryAPI :<|> ChatPostMessageAPI :<|> ReactionsAddAPI)

-- https://slack.com/api/channels.history
type ChannelsHistoryAPI =
  "channels.history"
    :> QueryParam "token" Token
    :> QueryParam "channel" Channel
    :> QueryParam "latest" Timestamp
    :> QueryParam "oldest" Timestamp
    :> QueryParam "count" Int
    :> QueryParam "inclusive" Bool
    :> Get '[JSON] (Result ChannelsHistory)

-- https://slack.com/api/chat.postMessage
type ChatPostMessageAPI =
  "chat.postMessage"
    :> Header "Authorization" Token
    :> ReqBody '[JSON] ChatPostMessageRequest
    :> Post '[JSON] (Result ChatPostMessage)

-- https://slack.com/api/reactions.add
type ReactionsAddAPI =
  "reactions.add"
    :> Header "Authorization" Token
    :> ReqBody '[JSON] ReactionsAddRequest
    :> Post '[JSON] (Result ReactionsAdd)

newtype Result a = Result { getResult :: Either String a }

instance FromJSON a => FromJSON (Result a) where
  parseJSON v@(Object o) = do
    ok <- o .: "ok"
    if ok
    then Result . Right <$> parseJSON v
    else Result . Left <$> o .: "error"
  parseJSON invalid = typeMismatch "Result a" invalid

data ReactionsAddRequest =
  ReactionsAddRequest
    { channel  :: Channel
    , ts       :: Timestamp
    , reaction :: Text
    }

instance ToJSON ReactionsAddRequest where
  toJSON ReactionsAddRequest{..} =
    object [ "channel"   .= channel
           , "timestamp" .= ts
           , "name"      .= reaction
           ]

data ReactionsAdd = ReactionsAdd

instance FromJSON ReactionsAdd where
  parseJSON _ = pure ReactionsAdd

data ChatPostMessageRequest =
  ChatPostMessageRequest
    { channel :: Channel
    , text    :: Text
    }

instance ToJSON ChatPostMessageRequest where
  toJSON ChatPostMessageRequest{..} =
    object [ "channel" .= channel
           , "text"    .= text
           ]

data ChatPostMessage =
  ChatPostMessage
    { channel :: Channel
    , ts      :: Timestamp
    }

instance FromJSON ChatPostMessage where
  parseJSON (Object o) =
    ChatPostMessage <$> o .: "channel"
                    <*> o .: "ts"
  parseJSON invalid    = typeMismatch "ChatPostMessage" invalid

newtype ChannelsHistory = ChannelsHistory { getMessages :: [Message] }

instance FromJSON ChannelsHistory where
  parseJSON (Object o) = ChannelsHistory <$> o .: "messages"
  parseJSON invalid    = typeMismatch "ChannelsHistory" invalid

api :: Proxy API
api = Proxy

lookupMessageClient :: Maybe Token
                    -> Maybe Channel
                    -> Maybe Timestamp
                    -> Maybe Timestamp
                    -> Maybe Int
                    -> Maybe Bool
                    -> ClientM (Result ChannelsHistory)
postMessageClient :: Maybe Token
                  -> ChatPostMessageRequest
                  -> ClientM (Result ChatPostMessage)
addReactionClient :: Maybe Token
                  -> ReactionsAddRequest
                  -> ClientM (Result ReactionsAdd)
lookupMessageClient :<|> postMessageClient :<|> addReactionClient = client api

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x
