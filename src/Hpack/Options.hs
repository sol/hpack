{-# LANGUAGE LambdaCase #-}
module Hpack.Options where

import           Control.Applicative
import           Control.Monad
import           Data.Maybe
import           System.FilePath
import           System.Directory

data ParseResult = Help | PrintVersion | PrintNumericVersion | Run ParseOptions | ParseError
  deriving (Eq, Show)

data Verbose = Verbose | NoVerbose
  deriving (Eq, Show)

data Force = Force | NoForce
  deriving (Eq, Show)

data ParseOptions = ParseOptions {
  parseOptionsVerbose :: Verbose
, parseOptionsForce :: Force
, parseOptionsHash :: Maybe Bool
, parseOptionsToStdout :: Bool
, parseOptionsTarget :: FilePath
} deriving (Eq, Show)

parseOptions :: FilePath -> [String] -> IO ParseResult
parseOptions defaultTarget = \ case
  ["--version"] -> return PrintVersion
  ["--numeric-version"] -> return PrintNumericVersion
  ["--help"] -> return Help
  args -> case targets of
    Right (target, toStdout) -> do
      file <- expandTarget defaultTarget target
      let
        options
          | toStdout = ParseOptions NoVerbose Force hash toStdout file
          | otherwise = ParseOptions verbose force hash toStdout file
      return (Run options)
    Left err -> return err
    where
      silentFlag = "--silent"
      forceFlags = ["--force", "-f"]
      hashFlag = "--hash"
      noHashFlag = "--no-hash"

      flags = hashFlag : noHashFlag : silentFlag : forceFlags

      verbose :: Verbose
      verbose = if silentFlag `elem` args then NoVerbose else Verbose

      force :: Force
      force = if any (`elem` args) forceFlags then Force else NoForce

      hash :: Maybe Bool
      hash = listToMaybe . reverse $ mapMaybe parse args
        where
          parse :: String -> Maybe Bool
          parse t = True <$ guard (t == hashFlag) <|> False <$ guard (t == noHashFlag)

      ys = filter (`notElem` flags) args

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
