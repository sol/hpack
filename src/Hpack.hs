{-# LANGUAGE CPP #-}
module Hpack (
  hpack
, hpackResult
, Result(..)
, Status(..)
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

import           Control.Monad.Compat
import qualified Data.ByteString as B
import           Data.List.Compat
import           Data.Maybe
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)
import           Data.Version (Version)
import qualified Data.Version as Version
import           System.Environment
import           System.Exit
import           System.IO
import           Text.ParserCombinators.ReadP

import           Paths_hpack (version)
import           Hpack.Config
import           Hpack.Run
import           Hpack.Util

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

hpackResult :: FilePath -> IO Result
hpackResult = hpackWithVersionResult version

data Result = Result {
  resultWarnings :: [String]
, resultCabalFile :: String
, resultStatus :: Status
}

data Status = Generated | AlreadyGeneratedByNewerHpack | OutputUnchanged

hpackWithVersion :: Version -> FilePath -> Bool -> IO ()
hpackWithVersion v dir verbose = do
    r <- hpackWithVersionResult v dir
    forM_ (resultWarnings r) $ \warning -> hPutStrLn stderr ("WARNING: " ++ warning)
    when verbose $ putStrLn $
      case resultStatus r of
        Generated -> "generated " ++ resultCabalFile r
        OutputUnchanged -> resultCabalFile r ++ " is up-to-date"
        AlreadyGeneratedByNewerHpack -> resultCabalFile r ++ " was generated with a newer version of hpack, please upgrade and try again."

hpackWithVersionResult :: Version -> FilePath -> IO Result
hpackWithVersionResult v dir = do
  (warnings, cabalFile, new) <- run dir
  old <- fmap splitHeader <$> tryReadFile cabalFile
  let oldVersion = fmap fst old >>= extractVersion
  status <-
    if (oldVersion <= Just v) then
      if (fmap snd old == Just (lines new)) then
        return OutputUnchanged
      else do
        B.writeFile cabalFile $ encodeUtf8 $ T.pack $ header v ++ new
        return Generated
    else
      return AlreadyGeneratedByNewerHpack
  return Result
    { resultWarnings = warnings
    , resultCabalFile = cabalFile
    , resultStatus = status
    }
  where
    splitHeader :: String -> ([String], [String])
    splitHeader = fmap (dropWhile null) . span ("--" `isPrefixOf`) . lines
