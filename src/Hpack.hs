{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
module Hpack (
  hpack
, hpackResult
, Result(..)
, Status(..)
, Verbose(..)
, Force(..)
, version
, main
, mainWith
#ifdef TEST
, header
, hpackWithVersionResult
#endif
) where

import           Control.Monad
import           Data.Version (Version)
import qualified Data.Version as Version
import           System.FilePath
import           System.Environment
import           System.Exit
import           System.IO (stderr)
import           Data.Aeson (Value)

import           Paths_hpack (version)
import           Hpack.Options
import           Hpack.Config
import           Hpack.Render
import           Hpack.Util
import           Hpack.Utf8 as Utf8
import           Hpack.CabalFile
import           Hpack.Yaml

programVersion :: Version -> String
programVersion v = "hpack version " ++ Version.showVersion v

header :: FilePath -> Version -> Hash -> String
header p v hash = unlines [
    "-- This file has been generated from " ++ takeFileName p ++ " by " ++ programVersion v ++ "."
  , "--"
  , "-- see: https://github.com/sol/hpack"
  , "--"
  , "-- hash: " ++ hash
  , ""
  ]

main :: IO ()
main = mainWith packageConfig decodeYaml

mainWith :: FilePath -> (FilePath -> IO (Either String Value)) -> IO ()
mainWith configFile decode = do
  result <- getArgs >>= parseOptions configFile
  case result of
    PrintVersion -> putStrLn (programVersion version)
    PrintNumericVersion -> putStrLn (Version.showVersion version)
    Help -> printHelp
    Run options -> case options of
      Options _verbose _force True file -> hpackStdOut (DecodeOptions file Nothing decode)
      Options verbose force False file -> hpack (DecodeOptions file Nothing decode) verbose force
    ParseError -> do
      printHelp
      exitFailure

printHelp :: IO ()
printHelp = do
  name <- getProgName
  Utf8.hPutStrLn stderr $ unlines [
      "Usage: " ++ name ++ " [ --silent ] [ --force | -f ] [ PATH ] [ - ]"
    , "       " ++ name ++ " --version"
    , "       " ++ name ++ " --help"
    ]

hpack :: DecodeOptions -> Verbose -> Force -> IO ()
hpack = hpackWithVersion version

hpackResult :: DecodeOptions -> Force -> IO Result
hpackResult = hpackWithVersionResult version

data Result = Result {
  resultWarnings :: [String]
, resultCabalFile :: String
, resultStatus :: Status
} deriving (Eq, Show)

data Status =
    Generated
  | ExistingCabalFileWasModifiedManually
  | AlreadyGeneratedByNewerHpack
  | OutputUnchanged
  deriving (Eq, Show)

hpackWithVersion :: Version -> DecodeOptions -> Verbose -> Force -> IO ()
hpackWithVersion v options verbose force = do
    r <- hpackWithVersionResult v options force
    printWarnings (resultWarnings r)
    when (verbose == Verbose) $ putStrLn $
      case resultStatus r of
        Generated -> "generated " ++ resultCabalFile r
        OutputUnchanged -> resultCabalFile r ++ " is up-to-date"
        AlreadyGeneratedByNewerHpack -> resultCabalFile r ++ " was generated with a newer version of hpack, please upgrade and try again."
        ExistingCabalFileWasModifiedManually -> resultCabalFile r ++ " was modified manually, please use --force to overwrite."

printWarnings :: [String] -> IO ()
printWarnings warnings = do
  forM_ warnings $ \warning -> Utf8.hPutStrLn stderr ("WARNING: " ++ warning)

mkStatus :: [String] -> Version -> CabalFile -> Status
mkStatus new v (CabalFile mOldVersion mHash old) = case (mOldVersion, mHash) of
  (Nothing, _) -> ExistingCabalFileWasModifiedManually
  (Just oldVersion, _) | oldVersion < makeVersion [0, 20, 0] -> Generated
  (_, Nothing) -> ExistingCabalFileWasModifiedManually
  (Just oldVersion, Just hash)
    | v < oldVersion -> AlreadyGeneratedByNewerHpack
    | sha256 (unlines old) /= hash -> ExistingCabalFileWasModifiedManually
    | old == new -> OutputUnchanged
    | otherwise -> Generated

hpackWithVersionResult :: Version -> DecodeOptions -> Force -> IO Result
hpackWithVersionResult v options force = do
  DecodeResult pkg cabalFile warnings <- readPackageConfig options >>= either die return
  oldCabalFile <- readCabalFile cabalFile
  let new = renderPackage (maybe [] cabalFileContents oldCabalFile) pkg
  let
    status = case force of
      Force -> Generated
      NoForce -> maybe Generated (mkStatus (lines new) v) oldCabalFile
  case status of
    Generated -> do
      let hash = sha256 new
      Utf8.writeFile cabalFile (header (decodeOptionsTarget options) v hash ++ new)
    _ -> return ()
  return Result {
      resultWarnings = warnings
    , resultCabalFile = cabalFile
    , resultStatus = status
    }

hpackStdOut :: DecodeOptions -> IO ()
hpackStdOut options = do
  DecodeResult pkg _cabalFile warnings <- readPackageConfig options >>= either die return
  Utf8.putStr (renderPackage [] pkg)
  printWarnings warnings
