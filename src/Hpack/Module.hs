{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
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
import           Data.Maybe
import           System.FilePath
import qualified System.Directory as Directory
import           Control.Monad
import           Data.List hiding (nub, sort)

import           Data.Aeson.Config.FromValue
import           Hpack.Util
import           Hpack.Haskell

import           Path (Path(..), PathComponent(..))
import qualified Path

newtype Module = Module {unModule :: String}
  deriving (Eq, Ord)

instance Show Module where
  show = show . unModule

instance IsString Module where
  fromString = Module

instance FromValue Module where
  fromValue = fmap Module . fromValue

toModule :: Path -> Module
toModule path = case reverse $ Path.components path of
  [] -> Module ""
  file : dirs -> Module . intercalate "." . reverse $ dropExtension file : dirs

getModules :: FilePath -> [FilePath] -> FilePath -> IO [Module]
getModules dir exclude literalSrc = sortModules <$> do
  exists <- Directory.doesDirectoryExist (dir </> literalSrc)
  if exists
    then do
      canonicalSrc <- Directory.canonicalizePath (dir </> literalSrc)

      let
        srcIsProjectRoot :: Bool
        srcIsProjectRoot = canonicalSrc == dir

        toModules :: [Path] -> [Module]
        toModules = removeSetup . nub . map toModule

        removeSetup :: [Module] -> [Module]
        removeSetup
          | srcIsProjectRoot = filter (/= "Setup")
          | otherwise = id

        stripSrc :: Path -> Maybe Path
        stripSrc
          | srcIsProjectRoot = Just
          | otherwise = Path.stripPrefix (Path.fromFilePath literalSrc)

        excludePaths :: [Path]
        excludePaths = mapMaybe (stripSrc . Path.fromFilePath) exclude

        shouldExclude :: Path -> Bool
        shouldExclude = (`elem` excludePaths)

      toModules . filter (not . shouldExclude) <$> getModuleFilesRecursive canonicalSrc
    else return []

sortModules :: [Module] -> [Module]
sortModules = map Module . sort . map unModule

isSourceFile :: PathComponent -> Bool
isSourceFile (splitExtension . unPathComponent -> (name, ext)) = ext `elem` extensions && isModuleNameComponent name
  where
    extensions :: [String]
    extensions = [
        ".hs"
      , ".lhs"
      , ".chs"
      , ".hsc"
      , ".y"
      , ".ly"
      , ".x"
      ]

isModuleComponent :: PathComponent -> Bool
isModuleComponent = isModuleNameComponent . unPathComponent

getModuleFilesRecursive :: FilePath -> IO [Path]
getModuleFilesRecursive baseDir = go (Path [])
  where
    addBaseDir :: Path -> FilePath
    addBaseDir = (baseDir </>) . Path.toFilePath

    listDirectory :: Path -> IO [PathComponent]
    listDirectory = fmap (map PathComponent) . Directory.listDirectory . addBaseDir

    doesFileExist :: Path -> IO Bool
    doesFileExist = Directory.doesFileExist . addBaseDir

    doesDirectoryExist :: Path -> IO Bool
    doesDirectoryExist = Directory.doesDirectoryExist . addBaseDir

    go :: Path -> IO [Path]
    go dir = do
      entries <- listDirectory dir

      files       <- filterWith doesFileExist      (filter isSourceFile      entries)
      directories <- filterWith doesDirectoryExist (filter isModuleComponent entries)

      subdirsFiles  <- concat <$> mapM go directories
      return (files ++ subdirsFiles)
      where
        filterWith :: (Path -> IO Bool) -> [PathComponent] -> IO [Path]
        filterWith p = filterM p . map addDir

        addDir :: PathComponent -> Path
        addDir entry = Path (unPath dir ++ [entry])
