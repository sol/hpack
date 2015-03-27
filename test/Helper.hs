module Helper where

import           Control.Exception
import           System.Directory

withFile :: String -> (FilePath -> IO a) -> IO a
withFile c action = bracket_ (writeFile file c) (removeFile file) (action file)
  where file = "foo.yaml"
