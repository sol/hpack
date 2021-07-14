{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}
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
, GenerateHashStrategy(..)

#ifdef TEST
, hpackResultWithVersion
, header
, renderCabalFile
#endif
) where

import           Imports

import           Data.Version (Version)
import qualified Data.Version as Version
import           System.FilePath
import           System.Environment
import           System.Exit
import           System.IO (stderr)
import           Data.Aeson (Value)
import           Data.Maybe

import           Paths_hpack (version)
import           Hpack.Options
import           Hpack.Config
import           Hpack.Render
import           Hpack.Util
import           Hpack.Utf8 as Utf8
import           Hpack.CabalFile

programVersion :: Maybe Version -> String
programVersion Nothing = "hpack"
programVersion (Just v) = "hpack version " ++ Version.showVersion v

header :: FilePath -> Maybe Version -> (Maybe Hash) -> [String]
header p v hash = [
    "-- This file has been generated from " ++ takeFileName p ++ " by " ++ programVersion v ++ "."
  , "--"
  , "-- see: https://github.com/sol/hpack"
  ] ++ case hash of
    Just h -> ["--" , "-- hash: " ++ h, ""]
    Nothing -> [""]

data Options = Options {
  optionsDecodeOptions :: DecodeOptions
, optionsForce :: Force
, optionsGenerateHashStrategy :: GenerateHashStrategy
, optionsToStdout :: Bool
}

data GenerateHashStrategy
  = ForceHash     -- ^ Option @--hash@ given.
  | ForceNoHash   -- ^ Option @--no-hash given.
  | PreferNoHash  -- ^ None of these option given, default behavior.
  deriving (Eq, Show)

getOptions :: FilePath -> [String] -> IO (Maybe (Verbose, Options))
getOptions defaultPackageConfig args = do
  result <- parseOptions defaultPackageConfig args
  case result of
    PrintVersion -> do
      putStrLn (programVersion $ Just version)
      return Nothing
    PrintNumericVersion -> do
      putStrLn (Version.showVersion version)
      return Nothing
    Help -> do
      printHelp
      return Nothing
    Run (ParseOptions verbose force hash toStdout file) -> do
      let generateHash = case hash of
            Just True -> ForceHash
            Just False -> ForceNoHash
            Nothing -> PreferNoHash
      return $ Just (verbose, Options defaultDecodeOptions {decodeOptionsTarget = file} force generateHash toStdout)
    ParseError -> do
      printHelp
      exitFailure

printHelp :: IO ()
printHelp = do
  name <- getProgName
  Utf8.hPutStrLn stderr $ unlines [
      "Usage: " ++ name ++ " [ --silent ] [ --force | -f ] [ --[no-]hash ] [ PATH ] [ - ]"
    , "       " ++ name ++ " --version"
    , "       " ++ name ++ " --numeric-version"
    , "       " ++ name ++ " --help"
    ]

hpack :: Verbose -> Options -> IO ()
hpack verbose options = hpackResult options >>= printResult verbose

defaultOptions :: Options
defaultOptions = Options defaultDecodeOptions NoForce PreferNoHash False

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

mkStatus :: CabalFile -> CabalFile -> Status
mkStatus new@(CabalFile _ mNewVersion mNewHash _) existing@(CabalFile _ mExistingVersion _ _)
  | new `hasSameContent` existing = OutputUnchanged
  | otherwise = case mExistingVersion of
      Nothing -> ExistingCabalFileWasModifiedManually
      Just _
        | mNewVersion < mExistingVersion -> AlreadyGeneratedByNewerHpack
        | isJust mNewHash && hashMismatch existing -> ExistingCabalFileWasModifiedManually
        | otherwise -> Generated

hasSameContent :: CabalFile -> CabalFile -> Bool
hasSameContent (CabalFile cabalVersionA _ _ a) (CabalFile cabalVersionB _ _ b) = cabalVersionA == cabalVersionB && a == b

hashMismatch :: CabalFile -> Bool
hashMismatch cabalFile = case cabalFileHash cabalFile of
  Nothing -> False
  Just hash -> hash /= calculateHash cabalFile

calculateHash :: CabalFile -> Hash
calculateHash (CabalFile cabalVersion _ _ body) = sha256 (unlines $ cabalVersion ++ body)

hpackResult :: Options -> IO Result
hpackResult = hpackResultWithVersion version

hpackResultWithVersion :: Version -> Options -> IO Result
hpackResultWithVersion v (Options options force generateHashStrategy toStdout) = do
  DecodeResult pkg (lines -> cabalVersion) cabalFileName warnings <- readPackageConfig options >>= either die return
  mExistingCabalFile <- readCabalFile cabalFileName
  let
    newCabalFile = makeCabalFile generateHashStrategy mExistingCabalFile cabalVersion v pkg

    status = case force of
      Force -> Generated
      NoForce -> maybe Generated (mkStatus newCabalFile) mExistingCabalFile

  case status of
    Generated -> writeCabalFile options toStdout cabalFileName newCabalFile
    _ -> return ()

  return Result {
    resultWarnings = warnings
  , resultCabalFile = cabalFileName
  , resultStatus = status
  }

writeCabalFile :: DecodeOptions -> Bool -> FilePath -> CabalFile -> IO ()
writeCabalFile options toStdout name cabalFile = do
  write . unlines $ renderCabalFile (decodeOptionsTarget options) cabalFile
  where
    write = if toStdout then Utf8.putStr else Utf8.writeFile name

makeCabalFile :: GenerateHashStrategy -> Maybe CabalFile -> [String] -> Version -> Package -> CabalFile
makeCabalFile strategy mExistingCabalFile cabalVersion v pkg = cabalFile
  where
    cabalFile = CabalFile cabalVersion (Just v) hash body

    hash
      | shouldGenerateHash mExistingCabalFile strategy = Just $ calculateHash cabalFile
      | otherwise = Nothing

    body = lines $ renderPackage (maybe [] cabalFileContents mExistingCabalFile) pkg

shouldGenerateHash :: Maybe CabalFile -> GenerateHashStrategy -> Bool
shouldGenerateHash mExistingCabalFile strategy = case (strategy, mExistingCabalFile) of
  (ForceHash, _) -> True
  (ForceNoHash, _) -> False
  (PreferNoHash, Nothing) -> False
  (PreferNoHash, Just CabalFile {cabalFileHash = Nothing}) -> False
  (PreferNoHash, Just CabalFile {cabalFileHash = Just _}) -> True

renderCabalFile :: FilePath -> CabalFile -> [String]
renderCabalFile file (CabalFile cabalVersion hpackVersion hash body) = cabalVersion ++ header file hpackVersion hash ++ body
