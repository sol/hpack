{-# LANGUAGE LambdaCase #-}
module Hpack.Options where

import           Control.Monad
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
, optionsConfigDir :: Maybe FilePath
, optionsConfigFile :: FilePath
} deriving (Eq, Show)

parseOptions :: FilePath -> [String] -> IO ParseResult
parseOptions defaultConfigFile xs = case xs of
  ["--version"] -> return PrintVersion
  ["--numeric-version"] -> return PrintNumericVersion
  ["--help"] -> return Help
  _ -> case targets of
    Just (target, toStdout) -> do
      (dir, file) <- splitDirectory defaultConfigFile target
      return $ Run (Options verbose force toStdout dir file)
    Nothing -> return ParseError
    where
      silentFlag = "--silent"
      forceFlags = ["--force", "-f"]

      flags = silentFlag : forceFlags

      verbose = if silentFlag `elem` xs then NoVerbose else Verbose
      force = if any (`elem` xs) forceFlags then Force else NoForce
      ys = filter (`notElem` flags) xs

      targets = case ys of
        ["-"] -> Just (Nothing, True)
        ["-", "-"] -> Nothing
        [dir] -> Just (Just dir, False)
        [dir, "-"] -> Just (Just dir, True)
        [] -> Just (Nothing, False)
        _ -> Nothing

splitDirectory :: FilePath -> Maybe FilePath -> IO (Maybe FilePath, FilePath)
splitDirectory defaultFileName = \ case
  Nothing -> return (Nothing, defaultFileName)
  (Just p) -> do
    isDirectory <- doesDirectoryExist p
    return $ if isDirectory
      then (Just p, defaultFileName)
      else let
        file = takeFileName p
        dir = takeDirectory p
        in (guard (p /= file) >> Just dir, if null file then defaultFileName else file)
