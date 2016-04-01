{-# LANGUAGE CPP #-}
module Hpack (
  hpack
, version
, main
#ifdef TEST
, hpackWithVersion
, parseVerbosity
, extractVersion
, parseVersion
#endif
) where

import           Prelude ()
import           Prelude.Compat

import           Control.DeepSeq
import           Control.Exception
import           Control.Monad.Compat
import           Data.List.Compat
import           Data.Maybe
import           Data.Version (Version)
import qualified Data.Version as Version
import           System.Environment
import           System.Exit
import           System.IO
import           System.IO.Error
import           Text.ParserCombinators.ReadP

import           Paths_hpack (version)
import           Hpack.Config
import           Hpack.Run

programVersion :: Version -> String
programVersion v = "hpack version " ++ Version.showVersion v

header :: Version -> String
header v = unlines [
    "-- This file has been generated from " ++ packageConfig ++ " by " ++ programVersion v ++ "."
  , "--"
  , "-- see: https://github.com/sol/hpack"
  , ""
  ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--version"] -> putStrLn (programVersion version)
    ["--help"] -> printHelp
    _ -> case parseVerbosity args of
      (verbose, [dir]) -> hpack dir verbose
      (verbose, []) -> hpack "" verbose
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

safeInit :: [a] -> [a]
safeInit [] = []
safeInit xs = init xs

extractVersion :: [String] -> Maybe Version
extractVersion = listToMaybe . mapMaybe (stripPrefix prefix >=> parseVersion . safeInit)
  where
    prefix = "-- This file has been generated from package.yaml by hpack version "

parseVersion :: String -> Maybe Version
parseVersion xs = case [v | (v, "") <- readP_to_S Version.parseVersion xs] of
  [v] -> Just v
  _ -> Nothing

hpack :: FilePath -> Bool -> IO ()
hpack = hpackWithVersion version

hpackWithVersion :: Version -> FilePath -> Bool -> IO ()
hpackWithVersion v dir verbose = do
  (warnings, name, new) <- run dir
  forM_ warnings $ \warning -> hPutStrLn stderr ("WARNING: " ++ warning)

  old <- either (const Nothing) (Just . splitHeader) <$> tryJust (guard . isDoesNotExistError) (readFile name >>= (return $!!))
  let oldVersion = fmap fst old >>= extractVersion

  if (oldVersion <= Just v) then do
    if (fmap snd old == Just (lines new)) then do
      output (name ++ " is up-to-date")
    else do
      (writeFile name $ header v ++ new)
      output ("generated " ++ name)
  else do
    output (name ++ " was generated with a newer version of hpack, please upgrade and try again.")
  where
    splitHeader :: String -> ([String], [String])
    splitHeader = fmap (dropWhile null) . span ("--" `isPrefixOf`) . lines

    output :: String -> IO ()
    output message
      | verbose = putStrLn message
      | otherwise = return ()
