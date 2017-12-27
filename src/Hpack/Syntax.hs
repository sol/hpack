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
import           Data.Data
import           System.FilePath.Posix (splitDirectories)

import           Hpack.Syntax.Util
import           Hpack.Syntax.UnknownFields
import           Hpack.Syntax.Git

data ParseDefaults = ParseDefaults {
  parseDefaultsGithub :: Github
, parseDefaultsRef :: Ref
, parseDefaultsPath :: Maybe Path
} deriving Generic

instance HasFieldNames ParseDefaults

instance FromJSON ParseDefaults where
  parseJSON = genericParseJSON

data Github = Github {
  githubUser :: String
, githubRepo :: String
}

instance FromJSON Github where
  parseJSON v = parseJSON v >>= parseGithub

parseGithub :: String -> Parser Github
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

data Ref = Ref {unRef :: String}

instance FromJSON Ref where
  parseJSON v = parseJSON v >>= parseRef

parseRef :: String -> Parser Ref
parseRef ref
  | isValidRef ref = return (Ref ref)
  | otherwise = fail ("invalid Git reference " ++ show ref)

data Path = Path {unPath :: [FilePath]}

instance FromJSON Path where
  parseJSON v = parseJSON v >>= parsePath
    where
      parsePath path
        | '\\' `elem` path = fail ("rejecting '\\' in " ++ show path ++ ", please use '/' to separate path components")
        | ':' `elem` path = fail ("rejecting ':' in " ++ show path)
        | "/" `elem` p = fail ("rejecting absolute path " ++ show path)
        | ".." `elem` p = fail ("rejecting \"..\" in " ++ show path)
        | otherwise = return (Path p)
        where
          p = splitDirectories path

data Defaults = Defaults {
  defaultsGithubUser :: String
, defaultsGithubRepo :: String
, defaultsRef :: String
, defaultsPath :: [FilePath]
} deriving (Eq, Show)

instance FromJSON Defaults where
  parseJSON v = toDefaults <$> case v of
    String _ -> parseJSON v >>= parseDefaultsFromString
    Object _ -> parseJSON v
    _ -> typeMismatch "Object or String" v
    where
      toDefaults :: ParseDefaults -> Defaults
      toDefaults ParseDefaults{..} = Defaults {
          defaultsGithubUser = githubUser parseDefaultsGithub
        , defaultsGithubRepo = githubRepo parseDefaultsGithub
        , defaultsRef = unRef parseDefaultsRef
        , defaultsPath = maybe [".hpack", "defaults.yaml"] unPath parseDefaultsPath
        }

parseDefaultsFromString :: String -> Parser ParseDefaults
parseDefaultsFromString xs = case break (== '@') xs of
  (github, '@' : ref) -> ParseDefaults <$> parseGithub github <*> parseRef ref <*> pure Nothing
  _ -> fail ("missing Git reference for " ++ show xs ++ ", the expected format is user/repo@ref")

instance HasFieldNames Defaults where
  fieldNames Proxy = fieldNames (Proxy :: Proxy ParseDefaults)
