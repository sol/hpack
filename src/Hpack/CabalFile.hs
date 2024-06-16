{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module Hpack.CabalFile (
  CabalFile(..)
, GitConflictMarkers(..)
, ExistingCabalFile
, NewCabalFile
, readCabalFile
, parseVersion
#ifdef TEST
, extractVersion
, removeGitConflictMarkers
#endif
) where

import           Imports

import           Data.Maybe
import           Data.Version (Version(..))
import qualified Data.Version as Version
import           Text.ParserCombinators.ReadP

import           Hpack.Util

data CabalFile a = CabalFile {
  cabalFileCabalVersion :: [String]
, cabalFileHpackVersion :: Maybe Version
, cabalFileHash :: Maybe Hash
, cabalFileContents :: [String]
, cabalFileGitConflictMarkers :: a
} deriving (Eq, Show)

data GitConflictMarkers = HasGitConflictMarkers | DoesNotHaveGitConflictMarkers
  deriving (Show, Eq)

type ExistingCabalFile = CabalFile GitConflictMarkers
type NewCabalFile = CabalFile ()

readCabalFile :: FilePath -> IO (Maybe ExistingCabalFile)
readCabalFile cabalFile = fmap parseCabalFile <$> tryReadFile cabalFile

parseCabalFile :: String -> ExistingCabalFile
parseCabalFile (lines -> input) = case span isComment <$> span (not . isComment) clean of
  (cabalVersion, (header, body)) -> CabalFile {
    cabalFileCabalVersion = cabalVersion
  , cabalFileHpackVersion = extractVersion header
  , cabalFileHash = extractHash header
  , cabalFileContents = dropWhile null body
  , cabalFileGitConflictMarkers = gitConflictMarkers
  }
  where
    clean :: [String]
    clean = removeGitConflictMarkers input

    gitConflictMarkers :: GitConflictMarkers
    gitConflictMarkers
      | input == clean = DoesNotHaveGitConflictMarkers
      | otherwise = HasGitConflictMarkers

    isComment :: String -> Bool
    isComment = ("--" `isPrefixOf`)

extractHash :: [String] -> Maybe Hash
extractHash = extract "-- hash: " Just

extractVersion :: [String] -> Maybe Version
extractVersion = extract prefix (stripFileName >=> parseVersion . safeInit)
  where
    prefix = "-- This file has been generated from "
    stripFileName :: String -> Maybe String
    stripFileName = listToMaybe . mapMaybe (stripPrefix " by hpack version ") . tails

extract :: String -> (String -> Maybe a) -> [String] -> Maybe a
extract prefix parse = listToMaybe . mapMaybe (stripPrefix prefix >=> parse)

safeInit :: [a] -> [a]
safeInit [] = []
safeInit xs = init xs

parseVersion :: String -> Maybe Version
parseVersion xs = case [v | (v, "") <- readP_to_S Version.parseVersion xs] of
  [v] -> Just v
  _ -> Nothing

removeGitConflictMarkers :: [String] -> [String]
removeGitConflictMarkers = takeBoth
  where
    takeBoth input = case break (isPrefixOf marker) input of
      (both, _marker : rest) -> both ++ takeOurs rest
      (both, []) -> both
      where
        marker = "<<<<<<< "

    takeOurs input = case break (== marker) input of
      (ours, _marker : rest) -> ours ++ dropTheirs rest
      (ours, []) -> ours
      where
        marker = "======="

    dropTheirs input = case break (isPrefixOf marker) input of
      (_theirs, _marker : rest) -> takeBoth rest
      (_theirs, []) -> []
      where
        marker = ">>>>>>> "
