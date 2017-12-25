{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
module Helper (
  module Test.Hspec
, module Test.Mockery.Directory
, module Control.Applicative
, withTempDirectory
, module System.FilePath
, withCurrentDirectory
, shouldParseAs
) where
import           Data.Yaml
import           Data.ByteString (ByteString)

import           Test.Hspec
import           Test.Mockery.Directory
import           Control.Applicative
import           System.Directory (getCurrentDirectory, setCurrentDirectory, canonicalizePath)
import           Control.Exception
import qualified System.IO.Temp as Temp
import           System.FilePath

withCurrentDirectory :: FilePath -> IO a -> IO a
withCurrentDirectory dir action = do
  bracket getCurrentDirectory setCurrentDirectory $ \ _ -> do
    setCurrentDirectory dir
    action

withTempDirectory :: (FilePath -> IO a) -> IO a
withTempDirectory action = Temp.withSystemTempDirectory "hspec" $ \dir -> do
  canonicalizePath dir >>= action

shouldParseAs :: (HasCallStack, Show a, Eq a, FromJSON a) => ByteString -> Either String a -> Expectation
shouldParseAs input expected = do
  decodeEither input `shouldBe` expected
