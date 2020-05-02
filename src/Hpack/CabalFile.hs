{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module Hpack.CabalFile where

import           Control.Monad
import           Data.List
import           Data.Maybe
import           Data.Version (Version(..))
import qualified Data.Version as Version
import           Text.ParserCombinators.ReadP

import           Hpack.Util

makeVersion :: [Int] -> Version
makeVersion v = Version v []

data CabalFile = CabalFile {
  cabalFileCabalVersion :: [String]
, cabalFileHpackVersion :: Maybe Version
, cabalFileHash :: Maybe Hash
, cabalFileContents :: [String]
} deriving (Eq, Show)

readCabalFile :: FilePath -> IO (Maybe CabalFile)
readCabalFile cabalFile = fmap parse <$> tryReadFile cabalFile
  where
    parse :: String -> CabalFile
    parse (splitHeader -> (cabalVersion, h, c)) = CabalFile cabalVersion (extractVersion h) (extractHash h) c

    splitHeader :: String -> ([String], [String], [String])
    splitHeader (removeGitConflictMarkers . lines -> c) =
      case span (not . isComment) c of
        (cabalVersion, xs) -> case span isComment xs of
          (header, body) -> (cabalVersion, header, dropWhile null body)

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
