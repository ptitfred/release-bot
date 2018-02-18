{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module SlackAPI.Types
  ( Challenge(..)
  , Event(..)
  , EventPayload(..)
  , EventResponse(..)
  , Token(..)
  , module SlackAPI.Events
  , ReleaseCommandPayload(..)
  , ReleaseCommandResponse(..)
  , Contrib(..)
  , Committer(..)
  , URL(..)
  ) where

import           SlackAPI.AesonUtils
import           SlackAPI.Events
import           Types

import           Data.Aeson
import           Data.Aeson.Types    (Parser, typeMismatch)
import           Data.String         (IsString (..))
import           Data.Text           (Text)
import           Web.FormUrlEncoded  (FromForm (..), parseMaybe, parseUnique)
import           Web.HttpApiData     (ToHttpApiData (..))

newtype Token = Token Text deriving (Monoid, IsString, ToHttpApiData, Show, ToJSON, FromJSON, Eq)

data EventPayload = UrlVerification Challenge Token
                  | EventCallback Event
                    deriving Show

instance FromJSON EventPayload where
  parseJSON = dispatchOnType parseEvents

parseEvents :: Type -> Value -> Parser EventPayload
parseEvents t@"url_verification" = withPayload t parseUrlVerification
parseEvents t@"event_callback"   = withPayload t parseEventCallback
parseEvents unknownType          = typeMismatch (payloadOfType unknownType)

withPayload :: Type -> (Object -> Parser a) -> Value -> Parser a
withPayload t = withObject (payloadOfType t)

payloadOfType :: Type -> String
payloadOfType = ofType "EventPayload"

type PayloadParser = Object -> Parser EventPayload

parseUrlVerification :: PayloadParser
parseUrlVerification o =
    UrlVerification <$> o .: "challenge"
                    <*> o .: "token"

parseEventCallback :: PayloadParser
parseEventCallback o = EventCallback <$> o .: "event"

data Event = ReactionAddedEvent ReactionAdded deriving Show

instance FromJSON Event where
  parseJSON = dispatchOnType parseCallbacks

parseCallbacks :: Type -> Value -> Parser Event
parseCallbacks "reaction_added" = fmap ReactionAddedEvent . parseJSON
parseCallbacks _                = typeMismatch "EventCallback"

newtype Challenge = Challenge Text deriving (Show, FromJSON, ToJSON)

data EventResponse = NoResponse
                   | Acknowledge { challenge :: Challenge }

instance ToJSON EventResponse where
  toJSON Acknowledge{..} = object [ "challenge" .= challenge ]
  toJSON NoResponse      = object []

data ReleaseCommandPayload = BadPayload
                           | ReleaseCommandPayload { projectName :: ProjectName
                                                   , userId      :: UserId
                                                   , channel     :: Channel
                                                   }

instance FromForm ReleaseCommandPayload where
  fromForm f = do
    let avoidEmpty (Just "") = Nothing
        avoidEmpty mt        = fromString <$> mt
        parseUserId  = fromString <$> parseUnique "user_id"    f
        parseChannel = fromString <$> parseUnique "channel_id" f
    pn <- avoidEmpty <$> parseMaybe "text" f
    case pn of
      Nothing   -> pure BadPayload
      Just name -> ReleaseCommandPayload name <$> parseUserId <*> parseChannel

data ReleaseCommandResponse = ChannelResponse { message :: Text }
                            | PrivateResponse { message :: Text }

instance ToJSON ReleaseCommandResponse where
  toJSON ChannelResponse{..} = message `as` "in_channel"
  toJSON PrivateResponse{..} = message `as` "ephemeral"

as :: Text -> Text -> Value
as text responseType =
  object [ "response_type" .= responseType
         , "text"          .= text
         ]

data Committer =
  Committer { githubLogin :: Text
            , slackId     :: UserId
            } deriving (Ord, Eq)

newtype URL = URL { getURL :: Text } deriving (IsString, Ord, Eq)

data Contrib =
  Contrib { committer :: Committer
          , title     :: Text
          , number    :: Int
          , url       :: URL
          }
