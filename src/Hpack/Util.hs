{-# LANGUAGE DeriveDataTypeable #-}
module Hpack.Util (
  List(..)
, GhcOption
, GhcProfOption
, GhcjsOption
, CppOption
, CcOption
, LdOption
, parseMain
, toModule
, getFilesRecursive
, tryReadFile
, expandGlobs
, sort
, lexicographically
) where

import           Prelude ()
import           Prelude.Compat

import           Control.Applicative
import           Control.Exception
import           Control.Monad.Compat
import           Data.Aeson.Types
import qualified Data.ByteString as B
import           Data.Char
import           Data.Data
import           Data.List.Compat hiding (sort)
import           Data.Ord
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8With)
import           Data.Text.Encoding.Error (lenientDecode)
import           System.IO.Error
import           System.Directory
import           System.FilePath
import qualified System.FilePath.Posix as Posix
import           System.FilePath.Glob

import           Hpack.Haskell

sort :: [String] -> [String]
sort = sortBy (comparing lexicographically)

lexicographically :: String -> (String, String)
lexicographically x = (map toLower x, x)

newtype List a = List {fromList :: [a]}
  deriving (Eq, Show, Data, Typeable)

instance FromJSON a => FromJSON (List a) where
  parseJSON v = List <$> case v of
    Array _ -> parseJSON v
    _ -> return <$> parseJSON v

type GhcOption = String
type GhcProfOption = String
type GhcjsOption = String
type CppOption = String
type CcOption = String
type LdOption = String

parseMain :: String -> (FilePath, [GhcOption])
parseMain main = case reverse name of
  x : _ | isQualifiedIdentifier name && x `notElem` ["hs", "lhs"] -> (intercalate "/" (init name) ++ ".hs", ["-main-is " ++ main])
  _ | isModule name -> (intercalate "/" name ++ ".hs", ["-main-is " ++ main])
  _ -> (main, [])
  where
    name = splitOn '.' main

splitOn :: Char -> String -> [String]
splitOn c = go
  where
    go xs = case break (== c) xs of
      (ys, "") -> [ys]
      (ys, _:zs) -> ys : go zs

toModule :: [FilePath] -> Maybe String
toModule path = case reverse path of
  [] -> Nothing
  x : xs -> do
    m <- stripSuffix ".hs" x <|> stripSuffix ".lhs" x <|> stripSuffix ".hsc" x
    let name = reverse (m : xs)
    guard (isModule name) >> return (intercalate "." name)
  where
    stripSuffix :: String -> String -> Maybe String
    stripSuffix suffix x = reverse <$> stripPrefix (reverse suffix) (reverse x)

getFilesRecursive :: FilePath -> IO [[String]]
getFilesRecursive baseDir = go []
  where
    go :: [FilePath] -> IO [[FilePath]]
    go dir = do
      c <- map ((dir ++) . return) . filter (`notElem` [".", ".."]) <$> getDirectoryContents (pathTo dir)
      subdirsFiles  <- filterM (doesDirectoryExist . pathTo) c >>= mapM go
      files <- filterM (doesFileExist . pathTo) c
      return (files ++ concat subdirsFiles)
      where
        pathTo :: [FilePath] -> FilePath
        pathTo p = baseDir </> joinPath p

tryReadFile :: FilePath -> IO (Maybe String)
tryReadFile file = do
  r <- tryJust (guard . isDoesNotExistError) (B.readFile file)
  return $ either (const Nothing) (Just . T.unpack . decodeUtf8With lenientDecode) r

toPosixFilePath :: FilePath -> FilePath
toPosixFilePath = Posix.joinPath . splitDirectories

expandGlobs :: String -> FilePath -> [String] -> IO ([String], [FilePath])
expandGlobs name dir patterns = do
  files <- (fst <$> globDir compiledPatterns dir) >>= mapM removeDirectories
  let warnings = [warn pattern | ([], pattern) <- zip files patterns]
  return (warnings, combineResults files)
  where
    combineResults = nub . sort . map (toPosixFilePath . makeRelative dir) . concat
    warn pattern = "Specified pattern " ++ show pattern ++ " for " ++ name ++ " does not match any files"
    compiledPatterns = map (compileWith options) patterns
    removeDirectories = filterM doesFileExist
    options = CompOptions {
        characterClasses = False
      , characterRanges = False
      , numberRanges = False
      , wildcards = True
      , recursiveWildcards = True
      , pathSepInRanges = False
      , errorRecovery = True
      }
