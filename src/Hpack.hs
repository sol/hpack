module Hpack (
  hpack
, version
, main
) where

import           Prelude ()
import           Prelude.Compat

import           Control.DeepSeq
import           Control.Exception
import           Control.Monad.Compat
import           Data.List.Compat
import           Data.Version (showVersion)
import           System.Environment
import           System.Exit
import           System.IO
import           System.IO.Error

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
  case args of
    ["--version"] -> putStrLn programVersion
    ["--silent"] -> hpack False
    [] -> hpack True
    _ -> do
      hPutStrLn stderr "Usage: hpack [ --version | --silent ]"
      exitFailure

hpack :: Bool -> IO ()
hpack verbose = do
  (warnings, name, new) <- run
  forM_ warnings $ \warning -> hPutStrLn stderr ("WARNING: " ++ warning)
  old <- force . either (const Nothing) (Just . stripHeader) <$> tryJust (guard . isDoesNotExistError) (readFile name)
  if (old == Just (lines new)) then do
    output (name ++ " is up-to-date")
  else do
    (writeFile name $ header ++ new)
    output ("generated " ++ name)
  where
    stripHeader :: String -> [String]
    stripHeader = dropWhile null . dropWhile ("--" `isPrefixOf`) . lines

    output :: String -> IO ()
    output message
      | verbose = putStrLn message
      | otherwise = return ()
