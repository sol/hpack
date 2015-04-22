module Helper (
  module Test.Hspec
, module Control.Applicative
, touch
) where

import           Test.Hspec
import           Control.Applicative
import           System.Directory
import           System.FilePath

touch :: FilePath -> IO ()
touch p = do
  createDirectoryIfMissing True (takeDirectory p)
  writeFile p ""
