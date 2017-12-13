module Hpack.Options where

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
, optionsTarget :: Maybe FilePath
} deriving (Eq, Show)

parseOptions :: [String] -> ParseResult
parseOptions xs = case xs of
  ["--version"] -> PrintVersion
  ["--numeric-version"] -> PrintNumericVersion
  ["--help"] -> Help
  _ -> case targets of
    Just (target, toStdout) -> Run (Options verbose force toStdout target)
    Nothing -> ParseError
    where
      silentFlag = "--silent"
      forceFlags = ["--force", "-f"]

      flags = [silentFlag] ++ forceFlags
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
