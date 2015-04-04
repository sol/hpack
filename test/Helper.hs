module Helper where

import           Control.Exception
import           System.Directory
import           System.FilePath
import           System.IO.Temp

inTempDirectory :: IO a -> IO a
inTempDirectory action = withSystemTempDirectory "hspec" $ \path -> do
  bracket getCurrentDirectory setCurrentDirectory $ \_ -> do
    setCurrentDirectory path
    action

touch :: FilePath -> IO ()
touch p = do
  createDirectoryIfMissing True (takeDirectory p)
  writeFile p ""
