{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Hpack.Util (
  List(..)
, fromMaybeList
, GhcOption
, GhcProfOption
, GhcjsOption
, CppOption
, CcOption
, LdOption
, parseMain
, toModule
, getModuleFilesRecursive
, tryReadFile
, expandGlobs
, sort
, lexicographically
, Hash
, sha256
) where

import           Control.Exception
import           Control.Monad
import           Data.Aeson.Types
import           Data.Char
import           Data.List hiding (sort)
import           Data.Ord
import           System.IO.Error
import           System.Directory
import           System.FilePath
import qualified System.FilePath.Posix as Posix
import           System.FilePath.Glob
import           Crypto.Hash

import           Hpack.Haskell
import           Hpack.Utf8 as Utf8

sort :: [String] -> [String]
sort = sortBy (comparing lexicographically)

lexicographically :: String -> (String, String)
lexicographically x = (map toLower x, x)

newtype List a = List {fromList :: [a]}
  deriving (Eq, Show, Functor, Foldable, Traversable, Monoid)

instance FromJSON a => FromJSON (List a) where
  parseJSON v = List <$> case v of
    Array _ -> parseJSON v
    _ -> return <$> parseJSON v

fromMaybeList :: Maybe (List a) -> [a]
fromMaybeList = maybe [] fromList

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
    m <- msum $ map (`stripSuffix` x) [
        ".hs"
      , ".lhs"
      , ".chs"
      , ".hsc"
      , ".y"
      , ".ly"
      , ".x"
      ]
    let name = reverse (m : xs)
    guard (isModule name) >> return (intercalate "." name)
  where
    stripSuffix :: String -> String -> Maybe String
    stripSuffix suffix x = reverse <$> stripPrefix (reverse suffix) (reverse x)

getModuleFilesRecursive :: FilePath -> IO [[String]]
getModuleFilesRecursive baseDir = go []
  where
    go :: [FilePath] -> IO [[FilePath]]
    go dir = do
      c <- map ((dir ++) . return) . filter (`notElem` [".", ".."]) <$> getDirectoryContents (pathTo dir)
      subdirsFiles  <- filterM (doesDirectoryExist . pathTo) c >>= mapM go . filter isModule
      files <- filterM (doesFileExist . pathTo) c
      return (files ++ concat subdirsFiles)
      where
        pathTo :: [FilePath] -> FilePath
        pathTo p = baseDir </> joinPath p

tryReadFile :: FilePath -> IO (Maybe String)
tryReadFile file = do
  r <- tryJust (guard . isDoesNotExistError) (Utf8.readFile file)
  return $ either (const Nothing) Just r

toPosixFilePath :: FilePath -> FilePath
toPosixFilePath = Posix.joinPath . splitDirectories

expandGlobs :: String -> FilePath -> [String] -> IO ([String], [FilePath])
expandGlobs name dir patterns = do
  files <- globDir_ compiledPatterns dir >>= mapM removeDirectories
  let warnings = [warn pattern | ([], pattern) <- zip files patterns]
  return (warnings, combineResults files)
  where
    globDir_ :: [Pattern] -> FilePath -> IO [[FilePath]]
#if MIN_VERSION_Glob(0,9,0)
    globDir_ = globDir
#else
    globDir_ xs = fmap fst . globDir xs
#endif
    combineResults :: [[FilePath]] -> [FilePath]
    combineResults = nub . sort . map (toPosixFilePath . makeRelative dir) . concat

    warn :: String -> String
    warn pattern = "Specified pattern " ++ show pattern ++ " for " ++ name ++ " does not match any files"

    compiledPatterns :: [Pattern]
    compiledPatterns = map (compileWith options) patterns

    removeDirectories :: [FilePath] -> IO [FilePath]
    removeDirectories = filterM doesFileExist

    options :: CompOptions
    options = CompOptions {
        characterClasses = False
      , characterRanges = False
      , numberRanges = False
      , wildcards = True
      , recursiveWildcards = True
      , pathSepInRanges = False
      , errorRecovery = True
      }

type Hash = String

sha256 :: String -> Hash
sha256 c = show (hash (Utf8.encodeUtf8 c) :: Digest SHA256)
