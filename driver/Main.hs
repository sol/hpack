module Main (main) where

import           Prelude ()
import           Prelude.Compat
import           Control.Monad.Compat
import           Control.Exception
import           System.IO
import           System.IO.Error
import           Data.List.Compat
import           Data.Version (showVersion)
import           Control.DeepSeq
import           System.Console.GetOpt (OptDescr(..), ArgDescr(..), ArgOrder(..), getOpt, usageInfo)
import           System.Environment (getArgs)
import           System.Exit (exitFailure, exitSuccess)

import           Paths_hpack (version)
import           Run

header :: String
header = unlines [
    "-- This file has been generated from " ++ configFile ++ " by hpack version " ++ showVersion version ++ "."
  , "--"
  , "-- see: https://github.com/sol/hpack"
  , ""
  ]

data Option = Help
  deriving (Show, Eq)

options :: [OptDescr Option]
options = [
    Option ['h'] ["help"] (NoArg Help) "show help and exit"
  ]

usage :: String
usage = usageInfo ("hpack " ++ showVersion version) options

main :: IO ()
main = do
  (opts, args, errs) <- fmap (getOpt Permute options) getArgs
  unless (null errs) (printUsage errs)
  unless (null args) (printUsage ["hpack does not accept non-option arguments\n"])
  when (Help `elem` opts) printHelp
  (warnings, name, new) <- run
  forM_ warnings $ \warning -> hPutStrLn stderr ("WARNING: " ++ warning)
  old <- force . either (const Nothing) (Just . stripHeader) <$> tryJust (guard . isDoesNotExistError) (readFile name)
  if old == Just (lines new)
    then putStrLn ("`" ++ name ++ "' is up-to-date.")
    else do
      putStrLn ("Writing `" ++ name ++ "' ...")
      writeFile name (header ++ new)
  where
    stripHeader :: String -> [String]
    stripHeader = dropWhile null . dropWhile ("--" `isPrefixOf`) . lines

printUsage :: [String] -> IO ()
printUsage errors = do
  hPutStrLn stderr (intercalate "\n" (usage : errors))
  exitFailure

printHelp :: IO ()
printHelp = do putStrLn usage; exitSuccess
