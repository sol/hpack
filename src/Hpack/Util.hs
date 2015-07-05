{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
module Hpack.Util (
  List(..)
, toModule
, getFilesRecursive
, tryReadFile
, sniffAlignment
, extractFieldOrderHint
, fromMaybeList
, expandGlobs
-- exported for testing
, splitField
) where

import           Control.Applicative
import           Control.Arrow (first)
import           Control.Monad
import           Control.Exception
import           Control.DeepSeq
import           Data.Char
import           Data.Maybe
import           Data.List hiding (find)
import           System.Directory
import           System.FilePath
import           System.FilePath.Glob (compile, globDir, match)
import           Data.Data

import           Data.Aeson.Types

newtype List a = List {fromList :: [a]}
  deriving (Eq, Show, Data, Typeable)

instance FromJSON a => FromJSON (List a) where
  parseJSON v = List <$> (parseJSON v <|> (return <$> parseJSON v))

toModule :: [FilePath] -> Maybe String
toModule path = case reverse path of
  [] -> Nothing
  x : xs -> do
    m <- stripSuffix ".hs" x <|> stripSuffix ".lhs" x
    let name = reverse (m : xs)
    guard (all isValidModuleName name) >> return (intercalate "." name)
  where
    stripSuffix :: String -> String -> Maybe String
    stripSuffix suffix x = reverse <$> stripPrefix (reverse suffix) (reverse x)

-- See `Cabal.Distribution.ModuleName` (http://git.io/bj34)
isValidModuleName :: String -> Bool
isValidModuleName [] = False
isValidModuleName (c:cs) = isUpper c && all isValidModuleChar cs

isValidModuleChar :: Char -> Bool
isValidModuleChar c = isAlphaNum c || c == '_' || c == '\''

getFilesRecursive :: FilePath -> IO [[FilePath]]
getFilesRecursive baseDir = sort <$> go []
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


fromMaybeList :: Maybe (List a) -> [a]
fromMaybeList = maybe [] fromList

expandGlobs :: [String] -> IO ([String], [FilePath])
expandGlobs [] = return ([], [])
expandGlobs allGlobs@(glob:globs) = do
  (matchedFirst, unmatchedFirst) <- do
    (ms, unms) <- first concat <$> globDir [compile glob] "."
    (,) <$> filterM doesFileExist ms <*> filterM doesFileExist unms

  let matchRemaining [] r = r
      matchRemaining (g:gs) r@(acceptedGlobs, matchedFiles_) =
         case filter (match $ compile g) (matchedFirst ++ unmatchedFirst) of
           [] -> matchRemaining gs r
           xs -> matchRemaining gs (g:acceptedGlobs, xs ++ matchedFiles_)

      (remainingGlobs, remainingFiles) = matchRemaining globs ([], [])
      unmatchedGlobs = allGlobs \\ ((if null matchedFirst then [] else [glob]) ++ remainingGlobs)

      matchedFiles = map (makeRelative ".") . sort . nub $ matchedFirst ++ remainingFiles

  return (formatMissingExtras unmatchedGlobs, matchedFiles)
  where
    formatMissingExtras :: [String] -> [String]
    formatMissingExtras = map f . sort
      where
        f name = "Specified extra-source-file " ++ show name
                 ++ " does not exist, skipping"
