module Hpack.Options where

import           Prelude ()
import           Prelude.Compat

data ParseResult = Help | PrintVersion | Run Options | ParseError
  deriving (Eq, Show)

data Options = Options {
  optionsVerbose :: Bool
, optionsToStdout :: Bool
, optionsTarget :: Maybe FilePath
} deriving (Eq, Show)

parseOptions :: [String] -> ParseResult
parseOptions xs = case xs of
  ["--version"] -> PrintVersion
  ["--help"] -> Help
  _ -> case targets of
    Just (target, toStdout) -> Run (Options verbose toStdout target)
    Nothing -> ParseError
    where
      silentFlag = "--silent"
      verbose = not (silentFlag `elem` xs)
      ys = filter (/= silentFlag) xs

      targets = case ys of
        ["-"] -> Just (Nothing, True)
        [dir] -> Just (Just dir, False)
        [dir, "-"] -> Just (Just dir, True)
        [] -> Just (Nothing, False)
        _ -> Nothing
