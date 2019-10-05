{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}
module Hpack.Syntax.BuildTools (
  BuildTools(..)
, ParseBuildTool(..)
, SystemBuildTools(..)
) where

import qualified Control.Monad.Fail as Fail
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
import           Hpack.Syntax.Dependencies (parseDependency)

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
      , parseListItem = objectDependency
      , parseDictItem = dependencyVersion
      , parseName = nameToBuildTool
      }

      nameToBuildTool :: Text -> ParseBuildTool
      nameToBuildTool (T.unpack -> name) = case break (== ':') name of
        (executable, "") -> UnqualifiedBuildTool executable
        (package, executable) -> QualifiedBuildTool package (drop 1 executable)

      buildToolFromString :: Text -> Parser (ParseBuildTool, DependencyVersion)
      buildToolFromString s = parseQualifiedBuildTool s <|> parseUnqualifiedBuildTool s

      parseQualifiedBuildTool :: Fail.MonadFail m => Text -> m (ParseBuildTool, DependencyVersion)
      parseQualifiedBuildTool = fmap fromCabal . cabalParse "build tool" . T.unpack
        where
          fromCabal :: D.ExeDependency -> (ParseBuildTool, DependencyVersion)
          fromCabal (D.ExeDependency package executable version) = (
              QualifiedBuildTool (D.unPackageName package) (D.unUnqualComponentName executable)
            , DependencyVersion Nothing $ versionConstraintFromCabal version
            )

      parseUnqualifiedBuildTool :: Fail.MonadFail m => Text -> m (ParseBuildTool, DependencyVersion)
      parseUnqualifiedBuildTool = fmap (first UnqualifiedBuildTool) . parseDependency "build tool"

newtype SystemBuildTools = SystemBuildTools {
  unSystemBuildTools :: Map String VersionConstraint
} deriving (Show, Eq, Semigroup, Monoid)

instance FromValue SystemBuildTools where
  fromValue = fmap (SystemBuildTools . Map.fromList) . parseDependencies parse
    where
      parse :: Parse String VersionConstraint
      parse = Parse {
        parseString = parseSystemBuildTool
      , parseListItem = (.: "version")
      , parseDictItem = versionConstraint
      , parseName = T.unpack
      }

      parseSystemBuildTool :: Fail.MonadFail m => Text -> m (String, VersionConstraint)
      parseSystemBuildTool = fmap fromCabal . cabalParse "system build tool" . T.unpack
        where
          fromCabal :: D.LegacyExeDependency -> (String, VersionConstraint)
          fromCabal (D.LegacyExeDependency name version) = (name, versionConstraintFromCabal version)
