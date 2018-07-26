{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}
module Hpack.Syntax.BuildTools (
  BuildTools(..)
, ParseBuildTool(..)
) where

import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Semigroup (Semigroup(..))
import           Data.Bifunctor
import           Control.Applicative
import qualified Distribution.Package as D
import qualified Data.Map.Lazy as Map

import qualified Distribution.Types.ExeDependency as D
import qualified Distribution.Types.UnqualComponentName as D

import           Data.Aeson.Config.FromValue

import           Hpack.Syntax.DependencyVersion
import           Hpack.Syntax.Dependency (parseDependency)

newtype BuildTools = BuildTools {
  unBuildTools :: [(ParseBuildTool, DependencyVersion)]
} deriving (Show, Eq, Semigroup, Monoid)

data ParseBuildTool = QualifiedBuildTool String String | UnqualifiedBuildTool String
  deriving (Show, Eq)

instance FromValue BuildTools where
  fromValue v = case v of
    String s -> BuildTools . return <$> buildToolFromString s
    Array xs -> BuildTools <$> parseArray buildToolFromValue xs
    Object _ -> BuildTools . map (first nameToBuildTool) . Map.toList <$> fromValue v
    _ -> typeMismatch "Array, Object, or String" v

nameToBuildTool :: String -> ParseBuildTool
nameToBuildTool name = case break (== ':') name of
  (executable, "") -> UnqualifiedBuildTool executable
  (package, executable) -> QualifiedBuildTool package (drop 1 executable)

buildToolFromValue :: Value -> Parser (ParseBuildTool, DependencyVersion)
buildToolFromValue v = case v of
  String s -> buildToolFromString s
  Object o -> sourceDependency o
  _ -> typeMismatch "Object or String" v
  where
    sourceDependency o = (,) <$> (nameToBuildTool <$> name) <*> (SourceDependency <$> fromValue v)
      where
        name :: Parser String
        name = o .: "name"

buildToolFromString :: Text -> Parser (ParseBuildTool, DependencyVersion)
buildToolFromString s = parseQualifiedBuildTool s <|> parseUnqualifiedBuildTool s

parseQualifiedBuildTool :: Monad m => Text -> m (ParseBuildTool, DependencyVersion)
parseQualifiedBuildTool = fmap f . cabalParse "build tool" . T.unpack
  where
    f :: D.ExeDependency -> (ParseBuildTool, DependencyVersion)
    f (D.ExeDependency package executable version) = (
        QualifiedBuildTool (D.unPackageName package) (D.unUnqualComponentName executable)
      , dependencyVersionFromCabal version
      )

parseUnqualifiedBuildTool :: Monad m => Text -> m (ParseBuildTool, DependencyVersion)
parseUnqualifiedBuildTool = fmap (first UnqualifiedBuildTool) . parseDependency "build tool"
