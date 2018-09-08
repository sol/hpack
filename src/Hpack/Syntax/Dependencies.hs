{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Hpack.Syntax.Dependencies (
  Dependencies(..)
, parseDependency
) where

import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Semigroup (Semigroup(..))
import qualified Distribution.Package as D
import           Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import           GHC.Exts

import           Data.Aeson.Config.FromValue

import           Hpack.Syntax.DependencyVersion
import           Hpack.Syntax.ParseDependencies

newtype Dependencies = Dependencies {
  unDependencies :: Map String DependencyVersion
} deriving (Eq, Show, Semigroup, Monoid)

instance IsList Dependencies where
  type Item Dependencies = (String, DependencyVersion)
  fromList = Dependencies . Map.fromList
  toList = Map.toList . unDependencies

instance FromValue Dependencies where
  fromValue = fmap (Dependencies . Map.fromList) . parseDependencies parse
    where
      parse :: Parse String DependencyVersion
      parse = Parse {
        parseString = parseDependency "dependency"
      , parseListItem = sourceDependency
      , parseDictItem = dependencyVersion
      , parseKey = T.unpack
      }

parseDependency :: Monad m => String -> Text -> m (String, DependencyVersion)
parseDependency subject = fmap fromCabal . cabalParse subject . T.unpack
  where
    fromCabal :: D.Dependency -> (String, DependencyVersion)
    fromCabal d = (D.unPackageName $ D.depPkgName d, VersionConstraint . versionConstraintFromCabal $ D.depVerRange d)
