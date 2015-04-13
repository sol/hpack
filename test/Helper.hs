module Helper (
  module Test.Hspec
, module Control.Applicative
, inTempDirectory
, touch
) where

import           Test.Hspec
import           Control.Applicative
import           Control.Exception
import           System.Directory
import           System.FilePath
import           System.IO.Temp

inTempDirectory :: FilePath -> IO a -> IO a
inTempDirectory name action = withSystemTempDirectory "hspec" $ \p -> do
  bracket getCurrentDirectory setCurrentDirectory $ \_ -> do
    let path = p </> name
    createDirectory path
    setCurrentDirectory path
    action

touch :: FilePath -> IO ()
touch p = do
  createDirectoryIfMissing True (takeDirectory p)
  writeFile p ""
