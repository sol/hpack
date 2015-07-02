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
import           System.Environment

import           Paths_hpack (version)
import           Hpack.Config
import           Hpack.Run

programVersion :: String
programVersion = "hpack version " ++ showVersion version

header :: String
header = unlines [
    "-- This file has been generated from " ++ packageConfig ++ " by " ++ programVersion ++ "."
  , "--"
  , "-- see: https://github.com/sol/hpack"
  , ""
  ]

main :: IO ()
main = do
  args <- getArgs
  if "--version" `elem` args
    then putStrLn programVersion
    else do
      (warnings, name, new) <- run
      forM_ warnings $ \warning -> hPutStrLn stderr ("WARNING: " ++ warning)
      old <- force . either (const Nothing) (Just . stripHeader) <$> tryJust (guard . isDoesNotExistError) (readFile name)
      unless (old == Just (lines new)) (writeFile name $ header ++ new)
      where
        stripHeader :: String -> [String]
        stripHeader = dropWhile null . dropWhile ("--" `isPrefixOf`) . lines
