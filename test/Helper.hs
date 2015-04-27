module Helper (
  module Test.Hspec
, module Test.Mockery.Directory
, module Control.Applicative
, touch
) where

import           Test.Hspec
import           Test.Mockery.Directory
import           Control.Applicative
import           System.Directory
import           System.FilePath

touch :: FilePath -> IO ()
touch p = do
  createDirectoryIfMissing True (takeDirectory p)
  writeFile p ""
