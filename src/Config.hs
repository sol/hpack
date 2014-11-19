{-# LANGUAGE DeriveGeneric #-}
module Config where

import           Data.Yaml
import           GHC.Generics

data Config = Config {
  dependencies :: [String]
} deriving (Eq, Show, Generic)

instance FromJSON Config

readConfig :: FilePath -> IO (Maybe Config)
readConfig = decodeFile
