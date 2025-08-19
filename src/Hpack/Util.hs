{-# LANGUAGE LambdaCase #-}
module Hpack.Util (
  GhcOption
, GhcProfOption
, GhcjsOption
, CppOption
, AsmOption
, CcOption
, CxxOption
, LdOption
, parseMain

, tryReadFile
, expandGlobs
, sort
, lexicographically
, Hash
, sha256

, nub
, nubOn
) where

import           Imports

import           Control.Exception
import           Data.Char
import           Data.Ord
import qualified Data.Set as Set
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

type GhcOption = String
type GhcProfOption = String
type GhcjsOption = String
type CppOption = String
type AsmOption = String
type CcOption = String
type CxxOption = String
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

tryReadFile :: FilePath -> IO (Maybe String)
tryReadFile file = do
  r <- tryJust (guard . isDoesNotExistError) (Utf8.readFile file)
  return $ either (const Nothing) Just r

toPosixFilePath :: FilePath -> FilePath
toPosixFilePath = Posix.joinPath . splitDirectories

data GlobResult = GlobResult {
  _globResultPattern :: String
, _globResultCompiledPattern :: Pattern
, _globResultFiles :: [FilePath]
}

expandGlobs :: String -> FilePath -> [String] -> IO ([String], [FilePath])
expandGlobs name dir patterns = do
  files <- globDir compiledPatterns dir >>= mapM removeDirectories
  badFiles <- globDir compiledExcludes dir >>= mapM removeDirectories
  let
     
    results :: [GlobResult]
    results = map (uncurry $ uncurry GlobResult) $ zip (zip patterns compiledPatterns) (map sort files)
    exclusions :: [GlobResult] 
    exclusions = map (uncurry $ uncurry GlobResult) $ zip (zip patterns compiledExcludes) (map sort badFiles)
  return (combineResults results exclusions)
  where 
    combineResults :: [GlobResult] -> [GlobResult] -> ([String], [FilePath])
    combineResults inc exc = 
      let
        
        (inwarn, include) = convertResults inc
        (exwarn, exclude) = convertResults exc
      in 
        (inwarn ++ exwarn, include \\ exclude)
    convertResults :: [GlobResult] -> ([String], [FilePath]) 
    convertResults = bimap concat (nub . concat) . unzip . map fromResult
    fromResult :: GlobResult -> ([String], [FilePath])
    fromResult (GlobResult pattern compiledPattern files) = case files of
      [] -> (warning, literalFile)
      xs -> ([], map normalize xs)
      where
        warning = [warn pattern compiledPattern]
        literalFile
          | isLiteral compiledPattern = [pattern]
          | otherwise = []

    normalize :: FilePath -> FilePath
    normalize = toPosixFilePath . makeRelative dir

    warn :: String -> Pattern -> String
    warn pattern compiledPattern
      | isLiteral compiledPattern = "Specified file " ++ show pattern ++ " for " ++ name ++ " does not exist"
      | otherwise = "Specified pattern " ++ show pattern ++ " for " ++ name ++ " does not match any files"
    compiledPatterns :: [Pattern]
    compiledPatterns = map fst $ filter (not . snd) compiledGlobs
    compiledExcludes :: [Pattern]
    compiledExcludes = map fst $ filter snd compiledGlobs
    compiledGlobs :: [(Pattern, Bool)]
    compiledGlobs = map compileHelper patterns
    
    compileHelper :: String -> (Pattern, Bool)
    compileHelper ('!':pattern) = (compileWith options pattern, True)
    compileHelper pattern       = (compileWith options pattern, False)
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

nub :: Ord a => [a] -> [a]
nub = nubOn id

nubOn :: Ord b => (a -> b) -> [a] -> [a]
nubOn f = go mempty
  where
    go seen = \ case
        [] -> []
        a : as
          | b `Set.member` seen -> go seen as
          | otherwise -> a : go (Set.insert b seen) as
          where
            b = f a
