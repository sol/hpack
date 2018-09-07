{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}
module Hpack.Syntax.BuildTools (
  BuildTools(..)
, ParseBuildTool(..)
, SystemBuildTools(..)
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
import qualified Distribution.Types.LegacyExeDependency as D

import           Data.Aeson.Config.FromValue

import           Hpack.Syntax.DependencyVersion
import           Hpack.Syntax.Dependency (parseDependency)

import           Hpack.Syntax.ParseDependencies

data ParseBuildTool = QualifiedBuildTool String String | UnqualifiedBuildTool String
  deriving (Show, Eq)

newtype BuildTools = BuildTools {
  unBuildTools :: [(ParseBuildTool, DependencyVersion)]
} deriving (Show, Eq, Semigroup, Monoid)

instance FromValue BuildTools where
  fromValue = fmap BuildTools . parseDependencies parse
    where
      parse :: Parse ParseBuildTool DependencyVersion
      parse = Parse {
        parseString = buildToolFromString
      , parseListItem = fmap SourceDependency . sourceDependency
      , parseDictItem = dependencyVersion
      , parseKey = nameToBuildTool
      }

nameToBuildTool :: Text -> ParseBuildTool
nameToBuildTool (T.unpack -> name) = case break (== ':') name of
  (executable, "") -> UnqualifiedBuildTool executable
  (package, executable) -> QualifiedBuildTool package (drop 1 executable)

buildToolFromString :: Text -> Parser (ParseBuildTool, DependencyVersion)
buildToolFromString s = parseQualifiedBuildTool s <|> parseUnqualifiedBuildTool s

parseQualifiedBuildTool :: Monad m => Text -> m (ParseBuildTool, DependencyVersion)
parseQualifiedBuildTool = fmap f . cabalParse "build tool" . T.unpack
  where
    f :: D.ExeDependency -> (ParseBuildTool, DependencyVersion)
    f (D.ExeDependency package executable version) = (
        QualifiedBuildTool (D.unPackageName package) (D.unUnqualComponentName executable)
      , VersionConstraint $ versionConstraintFromCabal version
      )

parseUnqualifiedBuildTool :: Monad m => Text -> m (ParseBuildTool, DependencyVersion)
parseUnqualifiedBuildTool = fmap (first UnqualifiedBuildTool) . parseDependency "build tool"

newtype SystemBuildTools = SystemBuildTools {
  unSystemBuildTools :: Map String DependencyVersion
} deriving (Show, Eq, Semigroup, Monoid)

instance FromValue SystemBuildTools where
  fromValue v = case v of
    String s -> fromList . return <$> parseSystemBuildTool s
    Array xs -> fromList <$> parseArray (withText parseSystemBuildTool) xs
    Object _ -> SystemBuildTools <$> fromValue v
    _ -> typeMismatch "Array, Object, or String" v
    where
      fromList :: [(String, DependencyVersion)] -> SystemBuildTools
      fromList = SystemBuildTools . Map.fromList

parseSystemBuildTool :: Monad m => Text -> m (String, DependencyVersion)
parseSystemBuildTool = fmap fromCabal . parseCabalBuildTool . T.unpack
  where
    fromCabal :: D.LegacyExeDependency -> (String, DependencyVersion)
    fromCabal (D.LegacyExeDependency name version) = (name, VersionConstraint $ versionConstraintFromCabal version)

    parseCabalBuildTool :: Monad m => String -> m D.LegacyExeDependency
    parseCabalBuildTool = cabalParse "system build tool"
