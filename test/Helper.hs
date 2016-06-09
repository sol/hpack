module Helper (
  module Test.Hspec
, module Test.Mockery.Directory
, module Control.Applicative
, withTempDirectory
, module System.FilePath
, withCurrentDirectory
) where

import           Test.Hspec
import           Test.Mockery.Directory
import           Control.Applicative
import qualified System.Directory as Dir
import           Control.Exception
import qualified System.IO.Temp as Temp
import           System.FilePath

withCurrentDirectory :: FilePath -> IO a -> IO a
withCurrentDirectory dir action = do
  bracket (Dir.getCurrentDirectory) (Dir.setCurrentDirectory) $ \ _ -> do
    Dir.setCurrentDirectory dir
    action

withTempDirectory :: (FilePath -> IO a) -> IO a
withTempDirectory action = Temp.withSystemTempDirectory "hspec" $ \dir -> do
  Dir.canonicalizePath dir >>= action
