{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}
module Hpack.Syntax.BuildTools (
  BuildTools(..)
, BuildTool(..)
) where

import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Semigroup (Semigroup(..))
import           Data.Bifunctor
import           Control.Applicative
import qualified Distribution.Package as D
import           Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map

import qualified Distribution.Types.ExeDependency as D
import qualified Distribution.Types.UnqualComponentName as D

import           Data.Aeson.Config.FromValue

import           Hpack.Syntax.DependencyVersion
import           Hpack.Syntax.Dependency (parseDependency)

newtype BuildTools = BuildTools {
  unBuildTools :: Map BuildTool DependencyVersion
} deriving (Show, Eq, Semigroup, Monoid)

data BuildTool = BuildTool {
  buildToolPackage :: String
, buildToolExecutable :: String
} deriving (Show, Eq, Ord)

instance FromValue BuildTools where
  fromValue v = case v of
    String s -> fromList . return <$> buildToolFromString s
    Array xs -> fromList <$> parseArray buildToolFromValue xs
    Object _ -> BuildTools . Map.mapKeys keyToBuildTool <$> fromValue v
    _ -> typeMismatch "Array, Object, or String" v
    where
      fromList :: [(BuildTool, DependencyVersion)] -> BuildTools
      fromList = BuildTools . Map.fromList

keyToBuildTool :: String -> BuildTool
keyToBuildTool name = case break (== ':') name of
  (executable, "") -> BuildTool executable executable
  (package, executable) -> BuildTool package (drop 1 executable)

buildToolFromValue :: Value -> Parser (BuildTool, DependencyVersion)
buildToolFromValue v = case v of
  String s -> buildToolFromString s
  Object o -> sourceDependency o
  _ -> typeMismatch "Object or String" v
  where
    sourceDependency o = (,) <$> (keyToBuildTool <$> name) <*> (SourceDependency <$> fromValue v)
      where
        name :: Parser String
        name = o .: "name"

buildToolFromString :: Text -> Parser (BuildTool, DependencyVersion)
buildToolFromString s = parseQualifiedBuildTool s <|> parseUnqualifiedBuildTool s

parseQualifiedBuildTool :: Monad m => Text -> m (BuildTool, DependencyVersion)
parseQualifiedBuildTool = fmap f . cabalParse "build tool" . T.unpack
  where
    f :: D.ExeDependency -> (BuildTool, DependencyVersion)
    f (D.ExeDependency package executable version) = (
        BuildTool (D.unPackageName package) (D.unUnqualComponentName executable)
      , dependencyVersionFromCabal version
      )

parseUnqualifiedBuildTool :: Monad m => Text -> m (BuildTool, DependencyVersion)
parseUnqualifiedBuildTool = fmap (first f) . parseDependency "build tool"
  where
    f :: String -> BuildTool
    f package = BuildTool package package
