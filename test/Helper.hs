{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Helper (
  module Test.Hspec
, module Test.Mockery.Directory
, module Control.Monad
, module Control.Applicative
, withTempDirectory
, module System.FilePath
, withCurrentDirectory
, yaml
, unknownField
) where

import           Test.Hspec
import           Test.Mockery.Directory
import           Data.String
import           Control.Monad
import           Control.Applicative
import           System.Directory (getCurrentDirectory, setCurrentDirectory, canonicalizePath)
import           Control.Exception
import qualified System.IO.Temp as Temp
import           System.FilePath

import           Data.Yaml.TH (yamlQQ)
import           Language.Haskell.TH.Quote (QuasiQuoter)

import           Hpack.Config
import           Data.Aeson.Config.FromValue

instance IsString Cond where
  fromString = CondExpression

withCurrentDirectory :: FilePath -> IO a -> IO a
withCurrentDirectory dir action = do
  bracket getCurrentDirectory setCurrentDirectory $ \ _ -> do
    setCurrentDirectory dir
    action

withTempDirectory :: (FilePath -> IO a) -> IO a
withTempDirectory action = Temp.withSystemTempDirectory "hspec" $ \dir -> do
  canonicalizePath dir >>= action

yaml :: Language.Haskell.TH.Quote.QuasiQuoter
yaml = yamlQQ

unknownField :: String -> Warning
unknownField path = Warning path UnknownField
