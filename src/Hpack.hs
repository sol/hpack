{-# LANGUAGE CPP #-}
module Hpack (
  hpack
, version
, main
#ifdef TEST
, parseVerbosity
#endif
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
    ["--help"] -> printHelp
    _ -> case parseVerbosity args of
      (verbose, [dir]) -> hpack dir verbose
      (verbose, []) -> hpack "." verbose
      _ -> do
        printHelp
        exitFailure

printHelp :: IO ()
printHelp = do
  hPutStrLn stderr $ unlines [
      "Usage: hpack [ --silent ] [ dir ]"
    , "       hpack --version"
    ]

parseVerbosity :: [String] -> (Bool, [String])
parseVerbosity xs = (verbose, ys)
  where
    silentFlag = "--silent"
    verbose = not (silentFlag `elem` xs)
    ys = filter (/= silentFlag) xs

hpack :: FilePath -> Bool -> IO ()
hpack dir verbose = do
  (warnings, name, new) <- run dir
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
