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
, optionsTarget :: FilePath
} deriving (Eq, Show)

parseOptions :: FilePath -> [String] -> IO ParseResult
parseOptions defaultTarget = \ case
  ["--version"] -> return PrintVersion
  ["--numeric-version"] -> return PrintNumericVersion
  ["--help"] -> return Help
  args -> parseRunOptions defaultTarget args

parseRunOptions :: FilePath -> [String] -> IO ParseResult
parseRunOptions defaultTarget xs = case targets of
  Right (target, toStdout) -> do
    file <- expandTarget defaultTarget target
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

expandTarget :: FilePath -> Maybe FilePath -> IO FilePath
expandTarget defaultTarget = \ case
  Nothing -> return defaultTarget
  Just "" -> return defaultTarget
  Just target -> do
    isFile <- doesFileExist target
    isDirectory <- doesDirectoryExist target
    return $ case takeFileName target of
      _ | isFile -> target
      _ | isDirectory -> target </> defaultTarget
      "" -> target </> defaultTarget
      _ -> target
