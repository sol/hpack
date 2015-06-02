module Main (main) where

import           Prelude ()
import           Prelude.Compat
import           Control.Monad.Compat
import           Control.Exception
import           System.IO
import           System.IO.Error
import           Data.List.Compat
import           Data.Version (showVersion)
import           Control.DeepSeq

import           Paths_hpack (version)
import           Run

header :: String
header = unlines [
    "-- This file has been generated from " ++ configFile ++ " by hpack version " ++ showVersion version ++ "."
  , "--"
  , "-- see: https://github.com/sol/hpack"
  , ""
  ]

main :: IO ()
main = do
  (warnings, name, new) <- run
  forM_ warnings $ \warning -> hPutStrLn stderr ("WARNING: " ++ warning)
  old <- force . either (const Nothing) (Just . stripHeader) <$> tryJust (guard . isDoesNotExistError) (readFile name)
  unless (old == Just (lines new)) (writeFile name $ header ++ new)
  where
    stripHeader :: String -> [String]
    stripHeader = dropWhile null . dropWhile ("--" `isPrefixOf`) . lines
