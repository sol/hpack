{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Hpack.Module (
  Module(..)
, toModule
, getModules
#ifdef TEST
, getModuleFilesRecursive
#endif
) where

import           Data.String
import           System.FilePath
import           System.Directory
import           Control.Monad
import           Data.List hiding (sort)
import           Data.Maybe

import           Data.Aeson.Config.FromValue
import           Hpack.Util
import           Hpack.Haskell

getModules :: FilePath -> FilePath -> IO [Module]
getModules dir src_ = sortModules <$> do
  exists <- doesDirectoryExist (dir </> src_)
  if exists
    then do
      src <- canonicalizePath (dir </> src_)
      removeSetup src . toModules <$> getModuleFilesRecursive src
    else return []
  where
    toModules :: [[FilePath]] -> [Module]
    toModules = catMaybes . map toModule

    removeSetup :: FilePath -> [Module] -> [Module]
    removeSetup src
      | src == dir = filter (/= "Setup")
      | otherwise = id

sortModules :: [Module] -> [Module]
sortModules = map Module . sort . map unModule

newtype Module = Module {unModule :: String}
  deriving Eq

instance Show Module where
  show (Module m) = show m

instance FromValue Module where
  fromValue = fmap Module . fromValue

instance IsString Module where
  fromString = Module

toModule :: [FilePath] -> Maybe Module
toModule path = case reverse path of
  [] -> Nothing
  x : xs -> do
    m <- msum $ map (`stripSuffix` x) sourceFileExtensions
    let name = reverse (m : xs)
    guard (isModule name) >> return (Module $ intercalate "." name)
  where
    stripSuffix :: String -> String -> Maybe String
    stripSuffix suffix x = reverse <$> stripPrefix (reverse suffix) (reverse x)

sourceFileExtensions :: [String]
sourceFileExtensions = [
    ".hs"
  , ".lhs"
  , ".chs"
  , ".hsc"
  , ".y"
  , ".ly"
  , ".x"
  ]

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
