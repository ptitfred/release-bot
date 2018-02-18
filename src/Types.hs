{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types where

import           Data.Aeson  (FromJSON)
import           Data.String (IsString)
import           Data.Text   (Text)
import           Servant.API (ToHttpApiData (..))

newtype ProjectName = ProjectName { getProjectName :: Text } deriving (ToHttpApiData, IsString, Show, Eq)

newtype URL = URL { getURL :: Text } deriving (FromJSON, IsString, Ord, Eq, Show)
