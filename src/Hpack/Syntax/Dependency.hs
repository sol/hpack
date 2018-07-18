{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
module Hpack.Syntax.Dependency (
  Dependencies(..)
) where

import qualified Data.Text as T
import           Data.Semigroup (Semigroup(..))
import           Control.Monad
import qualified Distribution.Package as D
import           Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import           GHC.Exts

import           Data.Aeson.Config.FromValue

import           Hpack.Syntax.DependencyVersion

newtype Dependencies = Dependencies {
  unDependencies :: Map String DependencyVersion
} deriving (Eq, Show, Semigroup, Monoid)

instance IsList Dependencies where
  type Item Dependencies = (String, DependencyVersion)
  fromList = Dependencies . Map.fromList
  toList = Map.toList . unDependencies

instance FromValue Dependencies where
  fromValue v = case v of
    String _ -> dependenciesFromList . return <$> fromValue v
    Array _ -> dependenciesFromList <$> fromValue v
    Object _ -> Dependencies <$> fromValue v
    _ -> typeMismatch "Array, Object, or String" v
    where
      fromDependency :: Dependency -> (String, DependencyVersion)
      fromDependency (Dependency name version) = (name, version)

      dependenciesFromList :: [Dependency] -> Dependencies
      dependenciesFromList = Dependencies . Map.fromList . map fromDependency

data Dependency = Dependency {
  _dependencyName :: String
, _dependencyVersion :: DependencyVersion
} deriving (Eq, Show)

instance FromValue Dependency where
  fromValue v = case v of
    String s -> uncurry Dependency <$> parseDependency (T.unpack s)
    Object o -> addSourceDependency o
    _ -> typeMismatch "Object or String" v
    where
      addSourceDependency o = Dependency <$> name <*> (SourceDependency <$> fromValue v)
        where
          name :: Parser String
          name = o .: "name"

depPkgName :: D.Dependency -> String
depPkgName = D.unPackageName . D.depPkgName

parseDependency :: Monad m => String -> m (String, DependencyVersion)
parseDependency = liftM fromCabal . parseCabalDependency
  where
    fromCabal :: D.Dependency -> (String, DependencyVersion)
    fromCabal d = (depPkgName d, dependencyVersionFromCabal $ D.depVerRange d)

parseCabalDependency :: Monad m => String -> m D.Dependency
parseCabalDependency = cabalParse "dependency"
