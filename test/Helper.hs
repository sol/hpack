module Helper (
  module Test.Hspec
, module Test.Mockery.Directory
, module Control.Applicative
, module System.IO.Temp
, module System.FilePath
, withCurrentDirectory
) where

import           Test.Hspec
import           Test.Mockery.Directory
import           Control.Applicative
import           System.Directory
import           Control.Exception
import           System.IO.Temp
import           System.FilePath

withCurrentDirectory :: FilePath -> IO a -> IO a
withCurrentDirectory dir action = do
  bracket (getCurrentDirectory) (setCurrentDirectory) $ \ _ -> do
    setCurrentDirectory dir
    action
