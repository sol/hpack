{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Hpack.Syntax.Dependencies (
  Dependencies(..)
, DependencyInfo(..)
, parseDependency
) where

import qualified Control.Monad.Fail as Fail
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Semigroup (Semigroup(..))
import qualified Distribution.Package as D
import           Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import           GHC.Exts

import           Data.Aeson.Config.FromValue
import           Data.Aeson.Config.Types

import           Hpack.Syntax.DependencyVersion
import           Hpack.Syntax.ParseDependencies

newtype Dependencies = Dependencies {
  unDependencies :: Map String DependencyInfo
} deriving (Eq, Show, Semigroup, Monoid)

instance IsList Dependencies where
  type Item Dependencies = (String, DependencyInfo)
  fromList = Dependencies . Map.fromList
  toList = Map.toList . unDependencies

instance FromValue Dependencies where
  fromValue = fmap (Dependencies . Map.fromList) . parseDependencies parse
    where
      parse :: Parse String DependencyInfo
      parse = Parse {
        parseString = \ input -> do
          (name, version) <- parseDependency "dependency" input
          return (name, DependencyInfo [] version)
      , parseListItem = objectDependencyInfo
      , parseDictItem = dependencyInfo
      , parseName = T.unpack
      }

data DependencyInfo = DependencyInfo {
  dependencyInfoMixins :: [String]
, dependencyInfoVersion :: DependencyVersion
} deriving (Eq, Show)

addMixins :: Object -> DependencyVersion -> Parser DependencyInfo
addMixins o version = do
  mixinsMay <- o .:? "mixin"
  return $ DependencyInfo (fromMaybeList mixinsMay) version

objectDependencyInfo :: Object -> Parser DependencyInfo
objectDependencyInfo o = objectDependency o >>= addMixins o

dependencyInfo :: Value -> Parser DependencyInfo
dependencyInfo = withDependencyVersion (DependencyInfo []) addMixins

parseDependency :: Fail.MonadFail m => String -> Text -> m (String, DependencyVersion)
parseDependency subject = fmap fromCabal . cabalParse subject . T.unpack
  where
    fromCabal :: D.Dependency -> (String, DependencyVersion)
    fromCabal d = (D.unPackageName $ D.depPkgName d, DependencyVersion Nothing . versionConstraintFromCabal $ D.depVerRange d)
