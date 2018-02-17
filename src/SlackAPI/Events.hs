{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module SlackAPI.Events where

import           SlackAPI.AesonUtils

import           Data.Aeson
import           Data.Aeson.Types    (Parser, typeMismatch)
import           Data.String         (IsString)
import           Data.Text           (Text)
import           Web.HttpApiData     (ToHttpApiData (..))

type User = Text
type ItemUser = Text

newtype Timestamp = Timestamp Text deriving (IsString, ToJSON, FromJSON, Show)

instance ToHttpApiData Timestamp where
  toQueryParam (Timestamp t) = toQueryParam t

data ReactionAdded =
  ReactionAdded { user      :: User
                , reaction  :: Text
                , itemUser  :: Maybe ItemUser
                , item      :: Item
                , timestamp :: Timestamp
                } deriving Show


instance FromJSON ReactionAdded where
  parseJSON (Object o) =
    ReactionAdded <$> o .: "user"
                  <*> o .: "reaction"
                  <*> o .:? "item_user"
                  <*> o .: "item"
                  <*> (Timestamp <$> o .: "event_ts")
  parseJSON invalid = typeMismatch "ReactionAdded" invalid

newtype Channel = Channel Text deriving (IsString, Show, ToJSON, FromJSON, ToHttpApiData)
newtype File = File Text deriving (IsString, Show, FromJSON)
newtype FileComment = FileComment Text deriving (IsString, Show, FromJSON)

data Item = MessageItem Channel Timestamp
          | FileItem File
          | FileCommentItem File FileComment
  deriving Show

instance FromJSON Item where
  parseJSON v = dispatchOnType parseItem v

parseItem :: Type -> Value -> Parser Item
parseItem t@"message"      = withItem t parseMessageItem
parseItem t@"file"         = withItem t parseFileItem
parseItem t@"file_comment" = withItem t parseFileCommentItem
parseItem unknownType      = typeMismatch (itemOfType unknownType)

withItem :: Type -> (Object -> Parser a) -> Value -> Parser a
withItem t = withObject (itemOfType t)

itemOfType :: Type -> String
itemOfType = ofType "Item"

type ItemParser = Object -> Parser Item

parseMessageItem :: ItemParser
parseMessageItem o =
  MessageItem <$> o .: "channel"
              <*> o .: "ts"

parseFileItem :: ItemParser
parseFileItem o = FileItem <$> o .: "file"

parseFileCommentItem :: ItemParser
parseFileCommentItem o =
  FileCommentItem <$> o .: "file"
                  <*> o .: "file_comment"
