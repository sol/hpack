{-# LANGUAGE LambdaCase #-}
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
, cabalFileContents :: [String]
} deriving (Eq, Show)

readCabalFile :: FilePath -> IO CabalFile
readCabalFile cabalFile = fmap splitHeader <$> tryReadFile cabalFile >>= \ case
  Nothing -> return (CabalFile Nothing [])
  Just (v, c) -> return (CabalFile (extractVersion v) c)
  where
    splitHeader :: String -> ([String], [String])
    splitHeader = fmap (dropWhile null) . span ("--" `isPrefixOf`) . lines

extractVersion :: [String] -> Maybe Version
extractVersion = listToMaybe . mapMaybe (stripPrefix prefix >=> parseVersion . safeInit)
  where
    prefix = "-- This file has been generated from package.yaml by hpack version "

safeInit :: [a] -> [a]
safeInit [] = []
safeInit xs = init xs

parseVersion :: String -> Maybe Version
parseVersion xs = case [v | (v, "") <- readP_to_S Version.parseVersion xs] of
  [v] -> Just v
  _ -> Nothing
