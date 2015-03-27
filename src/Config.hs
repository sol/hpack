{-# LANGUAGE DeriveGeneric #-}
module Config where

import           Data.Yaml
import           GHC.Generics
import           Data.HashMap.Lazy (HashMap)

data Config = Config {
  dependencies :: [String]
, tests :: HashMap String Test
} deriving (Eq, Show, Generic)

instance FromJSON Config

data Test = Test {
  main :: FilePath
} deriving (Eq, Show, Generic)

instance FromJSON Test

readConfig :: FilePath -> IO (Maybe Config)
readConfig = decodeFile
