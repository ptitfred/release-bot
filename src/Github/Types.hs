{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Github.Types where

import           Types

import           Data.Aeson
import           Data.Aeson.Types (typeMismatch)
import           Data.String      (IsString)
import           Data.Text        (Text)
import           Servant.API      (ToHttpApiData (..))

newtype OrganizationName = OrganizationName Text deriving (ToHttpApiData, IsString, Show)
newtype BranchName       = BranchName       Text deriving (ToHttpApiData, IsString, Show)

newtype PullRequestTitle = PullRequestTitle Text deriving (FromJSON, Show)
newtype Sha              = Sha              Text deriving (FromJSON, Eq, Ord, Show)
newtype Login            = Login            Text deriving (FromJSON, Show)

newtype Range = Range (BranchName, BranchName)

instance ToHttpApiData Range where
  toQueryParam (Range (from, to)) = mconcat [toQueryParam from, "...", toQueryParam to]

data Commits =
  Commits
    { commits :: [Commit]
    }

instance FromJSON Commits where
  parseJSON (Object o) =
    Commits <$> o .: "commits"
  parseJSON invalid = typeMismatch "Commits" invalid

data Commit =
  Commit
    { sha :: Sha
    }

instance FromJSON Commit where
  parseJSON (Object o) =
    Commit <$> o .: "sha"
  parseJSON invalid = typeMismatch "Commit" invalid

data PullRequest =
  PullRequest
    { title       :: PullRequestTitle
    , mergeCommit :: Sha
    , author      :: Login
    , url         :: URL
    , number      :: Int
    } deriving Show

instance FromJSON PullRequest where
  parseJSON (Object o) = do
    user <- o .: "user"
    PullRequest <$> o .: "title"
                <*> o .: "merge_commit_sha"
                <*> user .: "login"
                <*> o .: "html_url"
                <*> o .: "number"
  parseJSON invalid = typeMismatch "PullRequest" invalid
