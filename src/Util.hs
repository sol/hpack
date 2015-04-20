{-# LANGUAGE FlexibleContexts #-}
module Util (
  List(..)
, toModule
, genericParseJSON_
, getFilesRecursive
, tryReadFile
, sniffAlignment
, extractFieldOrderHint

-- exported for testing
, splitField
) where

import           Control.Applicative
import           Control.Monad
import           Control.Exception
import           Control.DeepSeq
import           Data.Char
import           Data.Maybe
import           Data.List
import           System.Directory
import           System.FilePath

import           GHC.Generics
import           Data.Aeson.Types

genericParseJSON_ :: (Generic a, GFromJSON (Rep a)) => String -> Value -> Parser a
genericParseJSON_ name = genericParseJSON defaultOptions {fieldLabelModifier = camelTo '-' . drop (length name)}

newtype List a = List {fromList :: [a]}
  deriving (Eq, Show)

instance FromJSON a => FromJSON (List a) where
  parseJSON v = List <$> (parseJSON v <|> (return <$> parseJSON v))

toModule :: [FilePath] -> Maybe String
toModule path = case reverse path of
  [] -> Nothing
  x : xs -> do
    m <- stripSuffix ".hs" x
    return (intercalate "." . reverse $ m : xs)
  where
    stripSuffix :: String -> String -> Maybe String
    stripSuffix suffix x = reverse <$> stripPrefix (reverse suffix) (reverse x)

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
