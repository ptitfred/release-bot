{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}

module Github.Client
  ( listMergedPullRequestTitlesForRange
  , listMergedPullRequestTitlesForRangeCustom
  ) where

import           Github.Types
import           Types

import           Data.Proxy
import qualified Data.Set                as S (fromList, member)
import           Data.Text               (Text)
import           Network.HTTP.Client.TLS (newTlsManager)
import           Servant.API
import           Servant.Client

data Direction = Asc | Desc
data Sort = Updated
data State = Open | Closed

instance ToHttpApiData State where
  toQueryParam Open   = "open"
  toQueryParam Closed = "closed"

instance ToHttpApiData Sort where
  toQueryParam Updated = "updated"

instance ToHttpApiData Direction where
  toQueryParam Asc  = "asc"
  toQueryParam Desc = "desc"

type API = "repos"
                :> (ListPullRequests :<|> ListCommits)

type UserAgent = Text

-- List closed PRs in project `ptitfred/release-bot`:
-- GET https://api.github.com/repos/ptitfred/twitch-analytics-haskell/pulls?state=closed&sort=updated&direction=desc
-- { "title": "pouet"
-- , "merge_commit_sha": "ef2a2988c9dea062a6f20b4807045f53cc769d6f"
-- , "user": { "login": "ptitfred"
--           }
-- }
type ListPullRequests = Capture "orga"    OrganizationName
                     :> Capture "project" ProjectName
                     :> Header "User-Agent" UserAgent
                     :> "pulls"
                     :> QueryParam "state"     State
                     :> QueryParam "sort"      Sort
                     :> QueryParam "direction" Direction
                     :> Get '[JSON] [PullRequest]

-- List commits for project `ptitfred/release-bot` between branch `master` and `development`:
-- GET https://api.github.com/repos/ptitfred/release-bot/compare/master...development
-- { commits: [ { "sha": "ef2a2988c9dea062a6f20b4807045f53cc769d6f"
--              }
--            ]
-- }
type ListCommits = Capture "orga"    OrganizationName
                :> Capture "project" ProjectName
                :> Header "User-Agent" UserAgent
                :> "compare"
                :> Capture "range" Range
                :> Get '[JSON] Commits

listPullRequestsClient :: OrganizationName -> ProjectName -> Maybe UserAgent -> Maybe State -> Maybe Sort -> Maybe Direction -> ClientM [PullRequest]
listCommitsForRangeClient :: OrganizationName -> ProjectName -> Maybe UserAgent -> Range -> ClientM Commits
listPullRequestsClient :<|> listCommitsForRangeClient = client (Proxy :: Proxy API)

listClosedPullRequestsClient :: OrganizationName -> ProjectName -> ClientM [PullRequest]
listClosedPullRequestsClient on pn = listPullRequestsClient on pn userAgent (Just Closed) (Just Updated) (Just Desc)

listClosedPullRequestsUpToClient :: OrganizationName -> ProjectName -> Commits -> ClientM [PullRequest]
listClosedPullRequestsUpToClient on pn Commits{..} =
  let shas = S.fromList (sha <$> commits)
      isAPullRequest pr = mergeCommit pr `S.member` shas
  in filter isAPullRequest <$> listClosedPullRequestsClient on pn

listClosedPullRequestsForRangeClient :: OrganizationName -> ProjectName -> Range -> ClientM [PullRequest]
listClosedPullRequestsForRangeClient on pn range =
  listCommitsForRangeClient on pn userAgent range >>= listClosedPullRequestsUpToClient on pn

listMergedPullRequestTitlesForRange :: OrganizationName -> ProjectName -> BranchName -> BranchName -> IO (Either ClientError [PullRequest])
listMergedPullRequestTitlesForRange on pn from to =
  defaultGithubClientEnv >>= listMergedPullRequestTitlesForRangeCustom on pn from to

listMergedPullRequestTitlesForRangeCustom :: OrganizationName -> ProjectName -> BranchName -> BranchName -> ClientEnv -> IO (Either ClientError [PullRequest])
listMergedPullRequestTitlesForRangeCustom on pn from to = runClientM (listClosedPullRequestsForRangeClient on pn range)
  where
    range = Range (from, to)

userAgent :: Maybe UserAgent
userAgent = Just "Release-Bot v1"

defaultGithubClientEnv :: IO ClientEnv
defaultGithubClientEnv =
  mkClientEnv <$> newTlsManager
            <*> pure githubApiv3

githubApiv3 :: BaseUrl
githubApiv3 =
  BaseUrl { baseUrlScheme = Https
          , baseUrlHost   = "api.github.com"
          , baseUrlPort   = 443
          , baseUrlPath   = ""
          }
