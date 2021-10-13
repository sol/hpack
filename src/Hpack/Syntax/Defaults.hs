{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Hpack.Syntax.Defaults (
  Defaults(..)
, Github(..)
, Local(..)
#ifdef TEST
, isValidOwner
, isValidRepo
#endif
) where

import           Imports

import           Data.Aeson.Config.KeyMap (member)
import qualified Data.Text as T
import           System.FilePath.Posix (splitDirectories)

import           Data.Aeson.Config.FromValue
import           Hpack.Syntax.Git

data ParseGithub = ParseGithub {
  parseGithubGithub :: GithubRepo
, parseGithubRef :: Ref
, parseGithubPath :: Maybe Path
} deriving (Generic, FromValue)

data GithubRepo = GithubRepo {
  githubRepoOwner :: String
, githubRepoName :: String
}

instance FromValue GithubRepo where
  fromValue = withString parseGithub

parseGithub :: String -> Parser GithubRepo
parseGithub github
  | not (isValidOwner owner) = fail ("invalid owner name " ++ show owner)
  | not (isValidRepo repo) = fail ("invalid repository name " ++ show repo)
  | otherwise = return (GithubRepo owner repo)
  where
    (owner, repo) = drop 1 <$> break (== '/') github

isValidOwner :: String -> Bool
isValidOwner owner =
     not (null owner)
  && all isAlphaNumOrHyphen owner
  && doesNotHaveConsecutiveHyphens owner
  && doesNotBeginWithHyphen owner
  && doesNotEndWithHyphen owner
  where
    isAlphaNumOrHyphen = (`elem` '-' : alphaNum)
    doesNotHaveConsecutiveHyphens = not . isInfixOf "--"
    doesNotBeginWithHyphen = not . isPrefixOf "-"
    doesNotEndWithHyphen = not . isSuffixOf "-"

isValidRepo :: String -> Bool
isValidRepo repo =
     not (null repo)
  && repo `notElem` [".", ".."]
  && all isValid repo
  where
    isValid = (`elem` '_' : '.' : '-' : alphaNum)

alphaNum :: [Char]
alphaNum = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

data Ref = Ref {unRef :: String}

instance FromValue Ref where
  fromValue = withString parseRef

parseRef :: String -> Parser Ref
parseRef ref
  | isValidRef ref = return (Ref ref)
  | otherwise = fail ("invalid Git reference " ++ show ref)

data Path = Path {unPath :: [FilePath]}

instance FromValue Path where
  fromValue = withString parsePath

parsePath :: String -> Parser Path
parsePath path
  | '\\' `elem` path = fail ("rejecting '\\' in " ++ show path ++ ", please use '/' to separate path components")
  | ':' `elem` path = fail ("rejecting ':' in " ++ show path)
  | "/" `elem` p = fail ("rejecting absolute path " ++ show path)
  | ".." `elem` p = fail ("rejecting \"..\" in " ++ show path)
  | otherwise = return (Path p)
  where
    p = splitDirectories path

data Github = Github {
  githubOwner :: String
, githubRepo :: String
, githubRef :: String
, githubPath :: [FilePath]
} deriving (Eq, Show)

toDefaultsGithub :: ParseGithub -> Github
toDefaultsGithub ParseGithub{..} = Github {
    githubOwner = githubRepoOwner parseGithubGithub
  , githubRepo = githubRepoName parseGithubGithub
  , githubRef = unRef parseGithubRef
  , githubPath = maybe [".hpack", "defaults.yaml"] unPath parseGithubPath
  }

parseDefaultsGithubFromString :: String -> Parser ParseGithub
parseDefaultsGithubFromString xs = case break (== '@') xs of
  (github, '@' : ref) -> ParseGithub <$> parseGithub github <*> parseRef ref <*> pure Nothing
  _ -> fail ("missing Git reference for " ++ show xs ++ ", the expected format is owner/repo@ref")

data Local = Local {
  localLocal :: String
} deriving (Eq, Show, Generic, FromValue)

data Defaults = DefaultsLocal Local | DefaultsGithub Github
  deriving (Eq, Show)

instance FromValue Defaults where
  fromValue v = case v of
    String s -> DefaultsGithub . toDefaultsGithub <$> parseDefaultsGithubFromString (T.unpack s)
    Object o | "local" `member` o -> DefaultsLocal <$> fromValue v
    Object o | "github" `member` o -> DefaultsGithub . toDefaultsGithub <$> fromValue v
    Object _ -> fail "neither key \"github\" nor key \"local\" present"
    _ -> typeMismatch "Object or String" v
