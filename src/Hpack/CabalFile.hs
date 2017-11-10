{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Hpack.CabalFile where

import           Prelude ()
import           Prelude.Compat

import           Control.Monad.Compat
import           Data.List.Compat
import           Data.Maybe
import           Data.Version (Version(..))
import qualified Data.Version as Version
import           Text.ParserCombinators.ReadP

import           Hpack.Util

makeVersion :: [Int] -> Version
makeVersion v = Version v []

data CabalFile = CabalFile {
  cabalFileHpackVersion :: Maybe Version
, cabalFileHash :: Maybe Hash
, cabalFileContents :: [String]
} deriving (Eq, Show)

modifiedManually :: CabalFile -> Bool
modifiedManually CabalFile{..} = case cabalFileHash of
  Nothing -> False
  Just hash -> sha256 (unlines cabalFileContents) /= hash

readCabalFile :: FilePath -> IO CabalFile
readCabalFile cabalFile = fmap splitHeader <$> tryReadFile cabalFile >>= \ case
  Nothing -> return (CabalFile Nothing Nothing [])
  Just (h, c) -> return (CabalFile (extractVersion h) (extractHash h) c)
  where
    splitHeader :: String -> ([String], [String])
    splitHeader = fmap (dropWhile null) . span ("--" `isPrefixOf`) . lines

extractHash :: [String] -> Maybe Hash
extractHash = extract "-- hash: " Just

extractVersion :: [String] -> Maybe Version
extractVersion = extract prefix (parseVersion . safeInit)
  where
    prefix = "-- This file has been generated from package.yaml by hpack version "

extract :: String -> (String -> Maybe a) -> [String] -> Maybe a
extract prefix parse = listToMaybe . mapMaybe (stripPrefix prefix >=> parse)

safeInit :: [a] -> [a]
safeInit [] = []
safeInit xs = init xs

parseVersion :: String -> Maybe Version
parseVersion xs = case [v | (v, "") <- readP_to_S Version.parseVersion xs] of
  [v] -> Just v
  _ -> Nothing
