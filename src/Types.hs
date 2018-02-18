{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types where

import           Data.String (IsString)
import           Data.Text   (Text)

newtype ProjectName = ProjectName { getProjectName :: Text } deriving (IsString, Eq)
