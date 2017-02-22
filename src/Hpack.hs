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
, splitDirectory
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
import           System.FilePath
import           System.Directory
import           Text.ParserCombinators.ReadP

import           Paths_hpack (version)
import           Hpack.Config
import           Hpack.Run
import           Hpack.Util

programVersion :: Version -> String
programVersion v = "hpack version " ++ Version.showVersion v

header :: FilePath -> Version -> String
header p v = unlines [
    "-- This file has been generated from " ++ p ++ " by " ++ programVersion v ++ "."
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
      (verbose, [dir]) -> hpack (Just dir) verbose
      (verbose, []) -> hpack Nothing verbose
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

hpack :: Maybe FilePath -> Bool -> IO ()
hpack = hpackWithVersion version

hpackResult :: Maybe FilePath -> IO Result
hpackResult = hpackWithVersionResult version

data Result = Result {
  resultWarnings :: [String]
, resultCabalFile :: String
, resultStatus :: Status
}

data Status = Generated | AlreadyGeneratedByNewerHpack | OutputUnchanged

hpackWithVersion :: Version -> Maybe FilePath -> Bool -> IO ()
hpackWithVersion v p verbose = do
    r <- hpackWithVersionResult v p
    forM_ (resultWarnings r) $ \warning -> hPutStrLn stderr ("WARNING: " ++ warning)
    when verbose $ putStrLn $
      case resultStatus r of
        Generated -> "generated " ++ resultCabalFile r
        OutputUnchanged -> resultCabalFile r ++ " is up-to-date"
        AlreadyGeneratedByNewerHpack -> resultCabalFile r ++ " was generated with a newer version of hpack, please upgrade and try again."

splitDirectory :: Maybe FilePath -> IO (Maybe FilePath, FilePath)
splitDirectory Nothing = return (Nothing, packageConfig)
splitDirectory (Just p) = do
  isDirectory <- doesDirectoryExist p
  return $ if isDirectory
    then (Just p, packageConfig)
    else let
      file = takeFileName p
      dir = takeDirectory p
      in (guard (p /= file) >> Just dir, if null file then packageConfig else file)

hpackWithVersionResult :: Version -> Maybe FilePath -> IO Result
hpackWithVersionResult v p = do
  (dir, file) <- splitDirectory p
  (warnings, cabalFile, new) <- run dir file
  old <- fmap splitHeader <$> tryReadFile cabalFile
  let oldVersion = fmap fst old >>= extractVersion
  status <-
    if (oldVersion <= Just v) then
      if (fmap snd old == Just (lines new)) then
        return OutputUnchanged
      else do
        B.writeFile cabalFile $ encodeUtf8 $ T.pack $ header file v ++ new
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
