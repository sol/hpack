{-# LANGUAGE DeriveGeneric #-}
module Config where

import           Data.Yaml
import           GHC.Generics
import           Data.HashMap.Lazy (HashMap)

import           Config.Test (Test)

data Config = Config {
  dependencies :: [String]
, tests :: HashMap String Test
} deriving (Eq, Show, Generic)

instance FromJSON Config

readConfig :: FilePath -> IO (Maybe Config)
readConfig = decodeFile
