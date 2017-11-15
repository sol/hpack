module Hpack.Options where

import           Prelude ()
import           Prelude.Compat

data ParseResult = Help | PrintVersion | Run Options | ParseError
  deriving (Eq, Show)

data Options = Options {
  optionsVerbose :: Bool
, optionsForce :: Bool
, optionsToStdout :: Bool
, optionsTarget :: Maybe FilePath
} deriving (Eq, Show)

parseOptions :: [String] -> ParseResult
parseOptions xs = case xs of
  ["--version"] -> PrintVersion
  ["--help"] -> Help
  _ -> case targets of
    Just (target, toStdout) -> Run (Options verbose force toStdout target)
    Nothing -> ParseError
    where
      silentFlag = "--silent"
      forceFlags = ["--force", "-f"]

      flags = [silentFlag] ++ forceFlags
      verbose = not (silentFlag `elem` xs)
      force = any (`elem` xs) forceFlags
      ys = filter (`notElem` flags) xs

      targets = case ys of
        ["-"] -> Just (Nothing, True)
        ["-", "-"] -> Nothing
        [dir] -> Just (Just dir, False)
        [dir, "-"] -> Just (Just dir, True)
        [] -> Just (Nothing, False)
        _ -> Nothing
