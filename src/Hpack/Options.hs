{-# LANGUAGE LambdaCase #-}
module Hpack.Options where

import           System.FilePath
import           System.Directory

data ParseResult = Help | PrintVersion | PrintNumericVersion | Run Options | ParseError
  deriving (Eq, Show)

data Verbose = Verbose | NoVerbose
  deriving (Eq, Show)

data Force = Force | NoForce
  deriving (Eq, Show)

data Options = Options {
  optionsVerbose :: Verbose
, optionsForce :: Force
, optionsToStdout :: Bool
, optionsConfigFile :: FilePath
} deriving (Eq, Show)

parseOptions :: FilePath -> [String] -> IO ParseResult
parseOptions defaultConfigFile = \ case
  ["--version"] -> return PrintVersion
  ["--numeric-version"] -> return PrintNumericVersion
  ["--help"] -> return Help
  args -> parseRunOptions defaultConfigFile args

parseRunOptions :: FilePath -> [String] -> IO ParseResult
parseRunOptions defaultConfigFile xs = case targets of
  Right (target, toStdout) -> do
    file <- expandConfigFile defaultConfigFile target
    return $ Run (Options verbose force toStdout file)
  Left err -> return err
  where
    silentFlag = "--silent"
    forceFlags = ["--force", "-f"]

    flags = silentFlag : forceFlags

    verbose = if silentFlag `elem` xs then NoVerbose else Verbose
    force = if any (`elem` xs) forceFlags then Force else NoForce
    ys = filter (`notElem` flags) xs

    targets :: Either ParseResult (Maybe FilePath, Bool)
    targets = case ys of
      ["-"] -> Right (Nothing, True)
      ["-", "-"] -> Left ParseError
      [path] -> Right (Just path, False)
      [path, "-"] -> Right (Just path, True)
      [] -> Right (Nothing, False)
      _ -> Left ParseError

expandConfigFile :: FilePath -> Maybe FilePath -> IO FilePath
expandConfigFile defaultConfigFile = \ case
  Nothing -> return defaultConfigFile
  Just "" -> return defaultConfigFile
  Just target -> do
    isFile <- doesFileExist target
    isDirectory <- doesDirectoryExist target
    return $ case takeFileName target of
      _ | isFile -> target
      _ | isDirectory -> target </> defaultConfigFile
      "" -> target </> defaultConfigFile
      _ -> target
