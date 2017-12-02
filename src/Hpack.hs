{-# LANGUAGE CPP #-}
module Hpack (
  hpack
, hpackResult
, Result(..)
, Status(..)
, Verbose(..)
, Force(..)
, version
, main
#ifdef TEST
, header
, hpackWithVersionResult
, splitDirectory
#endif
) where

import           Control.Monad
import qualified Data.ByteString as B
import           Data.Version (Version)
import qualified Data.Version as Version
import           System.Environment
import           System.Exit
import           System.IO
import           System.FilePath
import           System.Directory

import           Paths_hpack (version)
import           Hpack.Options
import           Hpack.Config
import           Hpack.Run
import           Hpack.Util
import           Hpack.CabalFile

programVersion :: Version -> String
programVersion v = "hpack version " ++ Version.showVersion v

header :: FilePath -> Version -> Hash -> String
header p v hash = unlines [
    "-- This file has been generated from " ++ p ++ " by " ++ programVersion v ++ "."
  , "--"
  , "-- see: https://github.com/sol/hpack"
  , "--"
  , "-- hash: " ++ hash
  , ""
  ]

main :: IO ()
main = do
  args <- getArgs
  case parseOptions args of
    PrintVersion -> putStrLn (programVersion version)
    Help -> printHelp
    Run options -> case options of
      Options _verbose _force True dir -> hpackStdOut dir
      Options verbose force False dir -> hpack dir verbose force
    ParseError -> do
      printHelp
      exitFailure

printHelp :: IO ()
printHelp = do
  hPutStrLn stderr $ unlines [
      "Usage: hpack [ --silent ] [ --force | -f ] [ PATH ] [ - ]"
    , "       hpack --version"
    , "       hpack --help"
    ]

hpack :: Maybe FilePath -> Verbose -> Force -> IO ()
hpack = hpackWithVersion version

hpackResult :: Maybe FilePath -> Force -> IO Result
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

hpackWithVersion :: Version -> Maybe FilePath -> Verbose -> Force -> IO ()
hpackWithVersion v p verbose force = do
    r <- hpackWithVersionResult v p force
    printWarnings (resultWarnings r)
    when (verbose == Verbose) $ putStrLn $
      case resultStatus r of
        Generated -> "generated " ++ resultCabalFile r
        OutputUnchanged -> resultCabalFile r ++ " is up-to-date"
        AlreadyGeneratedByNewerHpack -> resultCabalFile r ++ " was generated with a newer version of hpack, please upgrade and try again."
        ExistingCabalFileWasModifiedManually -> resultCabalFile r ++ " was modified manually, please use --force to overwrite."

printWarnings :: [String] -> IO ()
printWarnings warnings = do
  forM_ warnings $ \warning -> hPutStrLn stderr ("WARNING: " ++ warning)

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

hpackWithVersionResult :: Version -> Maybe FilePath -> Force -> IO Result
hpackWithVersionResult v p force = do
  (dir, file) <- splitDirectory p
  (warnings, cabalFile, new) <- run dir file
  oldCabalFile <- readCabalFile cabalFile
  let
    status = case force of
      Force -> Generated
      NoForce -> maybe Generated (mkStatus (lines new) v) oldCabalFile
  case status of
    Generated -> do
      let hash = sha256 new
      B.writeFile cabalFile . encodeUtf8 $ header file v hash ++ new
    _ -> return ()
  return Result
    { resultWarnings = warnings
    , resultCabalFile = cabalFile
    , resultStatus = status
    }

hpackStdOut :: Maybe FilePath -> IO ()
hpackStdOut p = do
  (dir, file) <- splitDirectory p
  (warnings, _cabalFile, new) <- run dir file
  B.putStr (encodeUtf8 new)
  printWarnings warnings
