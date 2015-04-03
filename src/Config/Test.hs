{-# LANGUAGE DeriveGeneric #-}
module Config.Test where

import           Data.Yaml
import           GHC.Generics

data Test = Test {
  main :: FilePath
, dependencies :: Maybe [String]
} deriving (Eq, Show, Generic)

instance FromJSON Test
