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
, splitDirectory
#endif
) where

import           Prelude ()
import           Prelude.Compat

import           Control.Monad.Compat
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as Encoding
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
import           Hpack.CabalFile

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
  case parseOptions args of
    PrintVersion -> putStrLn (programVersion version)
    Help -> printHelp
    Run options -> case options of
      Options _verbose True dir -> hpackStdOut dir
      Options verbose False dir -> hpack dir verbose
    ParseError -> do
      printHelp
      exitFailure

printHelp :: IO ()
printHelp = do
  hPutStrLn stderr $ unlines [
      "Usage: hpack [ --silent ] [ dir ] [ - ]"
    , "       hpack --version"
    ]

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
    printWarnings (resultWarnings r)
    when verbose $ putStrLn $
      case resultStatus r of
        Generated -> "generated " ++ resultCabalFile r
        OutputUnchanged -> resultCabalFile r ++ " is up-to-date"
        AlreadyGeneratedByNewerHpack -> resultCabalFile r ++ " was generated with a newer version of hpack, please upgrade and try again."

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

hpackWithVersionResult :: Version -> Maybe FilePath -> IO Result
hpackWithVersionResult v p = do
  (dir, file) <- splitDirectory p
  (warnings, cabalFile, new) <- run dir file
  CabalFile oldVersion old <- readCabalFile cabalFile
  status <-
    if (oldVersion <= Just v) then
      if (old == lines new) then
        return OutputUnchanged
      else do
        B.writeFile cabalFile . encodeUtf8 $ header file v ++ new
        return Generated
    else
      return AlreadyGeneratedByNewerHpack
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

encodeUtf8 :: String -> B.ByteString
encodeUtf8 = Encoding.encodeUtf8 . T.pack
