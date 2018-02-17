{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module SlackAPI.Types
  ( Challenge(..)
  , Event(..)
  , Payload(..)
  , Response(..)
  , Token(..)
  , module SlackAPI.Events
  ) where

import           SlackAPI.AesonUtils
import           SlackAPI.Events

import           Data.Aeson
import           Data.Aeson.Types    (Parser, typeMismatch)
import           Data.String         (IsString)
import           Data.Text           (Text)
import           Web.HttpApiData     (ToHttpApiData (..))

newtype Token = Token Text deriving (Monoid, IsString, ToHttpApiData, Show, ToJSON, FromJSON, Eq)

data Payload = UrlVerification Challenge Token
             | EventCallback Event
               deriving Show

instance FromJSON Payload where
  parseJSON = dispatchOnType parseEvents

parseEvents :: Type -> Value -> Parser Payload
parseEvents t@"url_verification" = withPayload t parseUrlVerification
parseEvents t@"event_callback"   = withPayload t parseEventCallback
parseEvents unknownType          = typeMismatch (payloadOfType unknownType)

withPayload :: Type -> (Object -> Parser a) -> Value -> Parser a
withPayload t = withObject (payloadOfType t)

payloadOfType :: Type -> String
payloadOfType = ofType "Payload"

type PayloadParser = Object -> Parser Payload

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

data Response = NoResponse
              | Acknowledge { challenge :: Challenge }

instance ToJSON Response where
  toJSON Acknowledge{..} = object [ "challenge" .= challenge ]
  toJSON NoResponse      = object []
