{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
module Hpack.Syntax (
  Defaults(..)
#ifdef TEST
, isValidUser
, isValidRepo
#endif
) where

import           Data.List
import           Data.Maybe
import           Data.Data

import           Hpack.Syntax.Util
import           Hpack.Syntax.UnknownFields

data ParseDefaults = ParseDefaults {
  parseDefaultsGithub :: Github
, parseDefaultsRef :: String
, parseDefaultsPath :: Maybe FilePath
} deriving Generic

data Github = Github {
  githubUser :: String
, githubRepo :: String
}

instance FromJSON Github where
  parseJSON v = parseJSON v >>= parseGithub
    where
      parseGithub github
        | not (isValidUser user) = fail ("invalid user name " ++ show user)
        | not (isValidRepo repo) = fail ("invalid repository name " ++ show repo)
        | otherwise = return (Github user repo)
        where
          (user, repo) = drop 1 <$> break (== '/') github

isValidUser :: String -> Bool
isValidUser user =
     not (null user)
  && all isAlphaNumOrHyphen user
  && doesNotHaveConsecutiveHyphens user
  && doesNotBeginWithHyphen user
  && doesNotEndWithHyphen user
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

instance HasFieldNames ParseDefaults

instance FromJSON ParseDefaults where
  parseJSON = genericParseJSON

data Defaults = Defaults {
  defaultsGithubUser :: String
, defaultsGithubRepo :: String
, defaultsRef :: String
, defaultsPath :: FilePath
} deriving (Eq, Show)

instance FromJSON Defaults where
  parseJSON v = toDefaults <$> parseJSON v
    where
      toDefaults :: ParseDefaults -> Defaults
      toDefaults ParseDefaults{..} = Defaults {
          defaultsGithubUser = githubUser parseDefaultsGithub
        , defaultsGithubRepo = githubRepo parseDefaultsGithub
        , defaultsRef = parseDefaultsRef
        , defaultsPath = fromMaybe ".hpack/defaults.yaml" parseDefaultsPath
        }

instance HasFieldNames Defaults where
  fieldNames Proxy = fieldNames (Proxy :: Proxy ParseDefaults)
