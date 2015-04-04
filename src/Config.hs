{-# LANGUAGE DeriveGeneric #-}
module Config where

import           Data.Yaml
import           GHC.Generics
import           Data.HashMap.Lazy (HashMap)

import           Util

data TestSection = TestSection {
  testSectionMain :: FilePath
, testSectionDependencies :: Maybe [String]
} deriving (Eq, Show, Generic)

instance FromJSON TestSection where
  parseJSON = genericParseJSON_ "TestSection"

data ConfigFile = ConfigFile {
  configFileName :: String
, configFileDependencies :: [String]
, configFileTests :: HashMap String TestSection
} deriving (Eq, Show, Generic)

instance FromJSON ConfigFile where
  parseJSON = genericParseJSON_ "ConfigFile"

readConfig :: FilePath -> IO (Maybe ConfigFile)
readConfig = decodeFile
