{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module SlackAPI.AesonUtils
  ( Type(..)
  , dispatchOnType
  , ofType
  ) where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.String      (IsString)
import           Data.Text        (Text, unpack)

newtype Type = Type Text deriving (IsString, Show, Eq)

getType :: Value -> Parser Type
getType (Object o) = Type <$> o .: "type"
getType invalid    = typeMismatch "Type" invalid

dispatchOnType :: (Type -> Value -> Parser v) -> Value -> Parser v
dispatchOnType parser v = getType v >>= flip parser v

ofType :: String -> Type -> String
ofType prefix (Type t) = prefix ++ "[type=" ++ unpack t ++ "]"
