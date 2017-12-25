{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
module Hpack.Syntax (
  Defaults(..)
) where

import           Data.Maybe
import           Data.Data

import           Hpack.Syntax.Util
import           Hpack.Syntax.UnknownFields

data ParseDefaults = ParseDefaults {
  parseDefaultsGithub :: String
, parseDefaultsRef :: String
, parseDefaultsPath :: Maybe FilePath
} deriving Generic

instance HasFieldNames ParseDefaults

instance FromJSON ParseDefaults where
  parseJSON = genericParseJSON

data Defaults = Defaults {
  defaultsGithub :: String
, defaultsRef :: String
, defaultsPath :: FilePath
} deriving (Eq, Show)

instance FromJSON Defaults where
  parseJSON v = toDefaults <$> parseJSON v
    where
      toDefaults :: ParseDefaults -> Defaults
      toDefaults ParseDefaults{..} = Defaults {
          defaultsGithub = parseDefaultsGithub
        , defaultsRef = parseDefaultsRef
        , defaultsPath = fromMaybe ".hpack/defaults.yaml" parseDefaultsPath
        }

instance HasFieldNames Defaults where
  fieldNames Proxy = fieldNames (Proxy :: Proxy ParseDefaults)
