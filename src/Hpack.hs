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
, parseBuildplan
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
import           Hpack.OptionalExposure (readBuildPlan, BuildPlan(..))
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
    _ -> case parseBuildplan args of
           (bp, args') -> case parseVerbosity args' of
                            (verbose, [dir]) -> hpack bp dir verbose
                            (verbose, []) -> hpack bp "" verbose
                            _ -> do
                              printHelp
                              exitFailure

printHelp :: IO ()
printHelp = do
  hPutStrLn stderr $ unlines [
      "Usage: hpack [ --silent ] [ --buildplan (min | file) ] [ dir ]"
    , "       hpack --version"
    ]

parseVerbosity :: [String] -> (Bool, [String])
parseVerbosity xs = (verbose, ys)
  where
    silentFlag = "--silent"
    verbose = not (silentFlag `elem` xs)
    ys = filter (/= silentFlag) xs

parseBuildplan :: [String] -> (Either BuildPlan FilePath, [String])
parseBuildplan = go id
  where go acc [] = (Left MaximalBuildPlan, acc [])
        go acc ("--buildplan":filePath:ys) =
          case filePath of
            "min" -> (Left MinimalBuildPlan, acc [] ++ ys)
            _ -> (Right filePath, acc [] ++ ys)
        go acc (y:ys) = go (acc . (y:)) ys

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

hpack :: Either BuildPlan FilePath -> FilePath -> Bool -> IO ()
hpack = hpackWithVersion version

hpackResult :: FilePath -> IO Result
hpackResult = hpackWithVersionResult version (Left MaximalBuildPlan)

data Result = Result {
  resultWarnings :: [String]
, resultCabalFile :: String
, resultStatus :: Status
}

data Status = Generated | AlreadyGeneratedByNewerHpack | OutputUnchanged

hpackWithVersion :: Version -> Either BuildPlan FilePath -> FilePath -> Bool -> IO ()
hpackWithVersion v buildPlan dir verbose = do
    r <- hpackWithVersionResult v buildPlan dir
    forM_ (resultWarnings r) $ \warning -> hPutStrLn stderr ("WARNING: " ++ warning)
    when verbose $ putStrLn $
      case resultStatus r of
        Generated -> "generated " ++ resultCabalFile r
        OutputUnchanged -> resultCabalFile r ++ " is up-to-date"
        AlreadyGeneratedByNewerHpack -> resultCabalFile r ++ " was generated with a newer version of hpack, please upgrade and try again."

hpackWithVersionResult :: Version -> Either BuildPlan FilePath ->  FilePath -> IO Result
hpackWithVersionResult v buildPlan dir = do
  let bpErr msg = putStrLn ("Error parsing build plan: "++msg) >> exitFailure
  buildPlan' <- either return (readBuildPlan >=> either bpErr return) buildPlan
  (warnings, cabalFile, new) <- run buildPlan' dir
  old <- either (const Nothing) (Just . splitHeader) <$> tryJust (guard . isDoesNotExistError) (readFile cabalFile >>= (return $!!))
  let oldVersion = fmap fst old >>= extractVersion
  status <-
    if (oldVersion <= Just v) then
      if (fmap snd old == Just (lines new)) then
        return OutputUnchanged
      else do
        writeFile cabalFile $ header v ++ new
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
