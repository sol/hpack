{-# LANGUAGE DeriveDataTypeable #-}
module Hpack.Util (
  List(..)
, GhcOption
, parseMain
, toModule
, getFilesRecursive
, tryReadFile
, sniffAlignment
, extractFieldOrderHint
, expandGlobs
, sort
, lexicographically

-- exported for testing
, splitField
) where

import           Control.Applicative
import           Control.DeepSeq
import           Control.Exception
import           Control.Monad
import           Data.Aeson.Types
import           Data.Char
import           Data.Data
import           Data.List hiding (sort)
import           Data.Maybe
import           Data.Ord
import           System.Directory
import           System.FilePath
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
    m <- stripSuffix ".hs" x <|> stripSuffix ".lhs" x
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
  r <- try (readFile file) :: IO (Either IOException String)
  return $!! either (const Nothing) Just r

extractFieldOrderHint :: String -> [String]
extractFieldOrderHint = map fst . catMaybes . map splitField . lines

sniffAlignment :: String -> Maybe Int
sniffAlignment input = case nub . catMaybes . map indentation . catMaybes . map splitField $ lines input of
  [n] -> Just n
  _ -> Nothing
  where

    indentation :: (String, String) -> Maybe Int
    indentation (name, value) = case span isSpace value of
      (_, "") -> Nothing
      (xs, _) -> (Just . succ . length $ name ++ xs)

splitField :: String -> Maybe (String, String)
splitField field = case span isNameChar field of
  (xs, ':':ys) -> Just (xs, ys)
  _ -> Nothing
  where
    isNameChar = (`elem` nameChars)
    nameChars = ['a'..'z'] ++ ['A'..'Z'] ++ "-"

expandGlobs :: [String] -> IO ([String], [FilePath])
expandGlobs patterns = do
  files <- (fst <$> globDir compiledPatterns ".") >>= mapM removeDirectories
  let warnings = [warn pattern | ([], pattern) <- zip files patterns]
  return (warnings, combineResults files)
  where
    combineResults = nub . map (makeRelative ".") . sort . concat
    warn pattern = "Specified pattern " ++ show pattern ++ " for extra-source-files does not match any files"
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
