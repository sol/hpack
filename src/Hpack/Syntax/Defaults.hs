{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Hpack.Syntax.Defaults (
  Defaults(..)
, DefaultsGithub(..)
, DefaultsLocal(..)
#ifdef TEST
, isValidOwner
, isValidRepo
#endif
) where

import           Data.HashMap.Lazy (member)
import           Data.List
import qualified Data.Text as T
import           System.FilePath.Posix (splitDirectories)

import           Data.Aeson.Config.FromValue
import           Hpack.Syntax.Git

data ParseDefaultsGithub = ParseDefaultsGithub {
  parseDefaultsGithubGithub :: Github
, parseDefaultsGithubRef :: Ref
, parseDefaultsGithubPath :: Maybe Path
} deriving (Generic, FromValue)

data Github = Github {
  githubOwner :: String
, githubRepo :: String
}

instance FromValue Github where
  fromValue = withString parseGithub

parseGithub :: String -> Parser Github
parseGithub github
  | not (isValidOwner owner) = fail ("invalid owner name " ++ show owner)
  | not (isValidRepo repo) = fail ("invalid repository name " ++ show repo)
  | otherwise = return (Github owner repo)
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

data DefaultsGithub = DefaultsGithub {
  defaultsGithubOwner :: String
, defaultsGithubRepo :: String
, defaultsGithubRef :: String
, defaultsGithubPath :: [FilePath]
} deriving (Eq, Show)

toDefaultsGithub :: ParseDefaultsGithub -> DefaultsGithub
toDefaultsGithub ParseDefaultsGithub{..} = DefaultsGithub {
    defaultsGithubOwner = githubOwner parseDefaultsGithubGithub
  , defaultsGithubRepo = githubRepo parseDefaultsGithubGithub
  , defaultsGithubRef = unRef parseDefaultsGithubRef
  , defaultsGithubPath = maybe [".hpack", "defaults.yaml"] unPath parseDefaultsGithubPath
  }

parseDefaultsGithubFromString :: String -> Parser ParseDefaultsGithub
parseDefaultsGithubFromString xs = case break (== '@') xs of
  (github, '@' : ref) -> ParseDefaultsGithub <$> parseGithub github <*> parseRef ref <*> pure Nothing
  _ -> fail ("missing Git reference for " ++ show xs ++ ", the expected format is owner/repo@ref")

data DefaultsLocal = DefaultsLocal {
  defaultsLocalLocal :: String
} deriving (Eq, Show, Generic, FromValue)

data Defaults = DefaultsLocal_ DefaultsLocal | DefaultsGithub_ DefaultsGithub
  deriving (Eq, Show)

instance FromValue Defaults where
  fromValue v = case v of
    String s -> DefaultsGithub_ . toDefaultsGithub <$> parseDefaultsGithubFromString (T.unpack s)
    Object o | "local" `member` o -> DefaultsLocal_ <$> fromValue v
    Object o | "github" `member` o -> DefaultsGithub_ . toDefaultsGithub <$> fromValue v
    Object _ -> fail "neither key \"github\" nor key \"local\" present"
    _ -> typeMismatch "Object or String" v
