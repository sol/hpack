{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
module Hpack (
-- | /__NOTE:__/ This module is exposed to allow integration of Hpack into
-- other tools.  It is not meant for general use by end users.  The following
-- caveats apply:
--
-- * The API is undocumented, consult the source instead.
--
-- * The exposed types and functions primarily serve Hpack's own needs, not
-- that of a public API.  Breaking changes can happen as Hpack evolves.
--
-- As an Hpack user you either want to use the @hpack@ executable or a build
-- tool that supports Hpack (e.g. @stack@ or @cabal2nix@).

-- * Version
  version

-- * Running Hpack
, hpack
, hpackResult
, printResult
, Result(..)
, Status(..)

-- * Options
, defaultOptions
, setProgramName
, setTarget
, setDecode
, getOptions
, Verbose(..)
, Options(..)
, Force(..)

#ifdef TEST
, hpackResultWithVersion
, header
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

data Options = Options {
  optionsDecodeOptions :: DecodeOptions
, optionsForce :: Force
, optionsToStdout :: Bool
}

getOptions :: FilePath -> [String] -> IO (Maybe (Verbose, Options))
getOptions defaultPackageConfig args = do
  result <- parseOptions defaultPackageConfig args
  case result of
    PrintVersion -> do
      putStrLn (programVersion version)
      return Nothing
    PrintNumericVersion -> do
      putStrLn (Version.showVersion version)
      return Nothing
    Help -> do
      printHelp
      return Nothing
    Run options -> case options of
      ParseOptions verbose force toStdout file -> do
        return $ Just (verbose, Options defaultDecodeOptions {decodeOptionsTarget = file} force toStdout)
    ParseError -> do
      printHelp
      exitFailure

printHelp :: IO ()
printHelp = do
  name <- getProgName
  Utf8.hPutStrLn stderr $ unlines [
      "Usage: " ++ name ++ " [ --silent ] [ --force | -f ] [ PATH ] [ - ]"
    , "       " ++ name ++ " --version"
    , "       " ++ name ++ " --numeric-version"
    , "       " ++ name ++ " --help"
    ]

hpack :: Verbose -> Options -> IO ()
hpack verbose options = hpackResult options >>= printResult verbose

defaultOptions :: Options
defaultOptions = Options defaultDecodeOptions NoForce False

setTarget :: FilePath -> Options -> Options
setTarget target options@Options{..} =
  options {optionsDecodeOptions = optionsDecodeOptions {decodeOptionsTarget = target}}

setProgramName :: ProgramName -> Options -> Options
setProgramName name options@Options{..} =
  options {optionsDecodeOptions = optionsDecodeOptions {decodeOptionsProgramName = name}}

setDecode :: (FilePath -> IO (Either String ([String], Value))) -> Options -> Options
setDecode decode options@Options{..} =
  options {optionsDecodeOptions = optionsDecodeOptions {decodeOptionsDecode = decode}}

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

printResult :: Verbose -> Result -> IO ()
printResult verbose r = do
  printWarnings (resultWarnings r)
  when (verbose == Verbose) $ putStrLn $
    case resultStatus r of
      Generated -> "generated " ++ resultCabalFile r
      OutputUnchanged -> resultCabalFile r ++ " is up-to-date"
      AlreadyGeneratedByNewerHpack -> resultCabalFile r ++ " was generated with a newer version of hpack, please upgrade and try again."
      ExistingCabalFileWasModifiedManually -> resultCabalFile r ++ " was modified manually, please use --force to overwrite."
  case resultStatus r of
      Generated -> return ()
      OutputUnchanged -> return ()
      AlreadyGeneratedByNewerHpack -> exitFailure
      ExistingCabalFileWasModifiedManually -> exitFailure

printWarnings :: [String] -> IO ()
printWarnings = mapM_ $ Utf8.hPutStrLn stderr . ("WARNING: " ++)

mkStatus :: [String] -> Version -> CabalFile -> Status
mkStatus new v (CabalFile mOldVersion mHash old) = case (mOldVersion, mHash) of
  (Nothing, _) -> ExistingCabalFileWasModifiedManually
  (Just oldVersion, _) | oldVersion < makeVersion [0, 20, 0] -> Generated
  (_, Nothing) -> ExistingCabalFileWasModifiedManually
  (Just oldVersion, Just hash)
    | old == new -> OutputUnchanged
    | v < oldVersion -> AlreadyGeneratedByNewerHpack
    | sha256 (unlines old) /= hash -> ExistingCabalFileWasModifiedManually
    | otherwise -> Generated

hpackResult :: Options -> IO Result
hpackResult = hpackResultWithVersion version

hpackResultWithVersion :: Version -> Options -> IO Result
hpackResultWithVersion v (Options options force toStdout) = do
  DecodeResult pkg cabalVersion cabalFile warnings <- readPackageConfig options >>= either die return
  oldCabalFile <- readCabalFile cabalFile
  let
    body = renderPackage (maybe [] cabalFileContents oldCabalFile) pkg
    withoutHeader = cabalVersion ++ body
  let
    status = case force of
      Force -> Generated
      NoForce -> maybe Generated (mkStatus (lines withoutHeader) v) oldCabalFile
  case status of
    Generated -> do
      let hash = sha256 withoutHeader
          out  = cabalVersion ++ header (decodeOptionsTarget options) v hash ++ body
      if toStdout
        then Utf8.putStr out
        else Utf8.writeFile cabalFile out
    _ -> return ()
  return Result {
      resultWarnings = warnings
    , resultCabalFile = cabalFile
    , resultStatus = status
    }
