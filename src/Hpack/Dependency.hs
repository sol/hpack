{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Hpack.Dependency (
  Dependencies(..)
, DependencyVersion(..)
, SourceDependency(..)
, GitRef
, GitUrl
, githubBaseUrl
) where

import           Prelude ()
import           Prelude.Compat

import qualified Data.Text as T
import           Text.PrettyPrint (renderStyle, Style(..), Mode(..))
import           Control.Monad
import qualified Distribution.Compat.ReadP as D
import qualified Distribution.Package as D
import qualified Distribution.Text as D
import qualified Distribution.Version as D
import           Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import           Data.Aeson.Types
import           Control.Applicative
import           GHC.Exts

githubBaseUrl :: String
githubBaseUrl = "https://github.com/"

newtype Dependencies = Dependencies {
  unDependencies :: Map String DependencyVersion
} deriving (Eq, Show, Monoid)

instance IsList Dependencies where
  type Item Dependencies = (String, DependencyVersion)
  fromList = Dependencies . Map.fromList
  toList = Map.toList . unDependencies

data DependencyVersion =
    AnyVersion
  | VersionRange String
  | SourceDependency SourceDependency
  deriving (Eq, Show)

data SourceDependency = GitRef GitUrl GitRef (Maybe FilePath) | Local FilePath
  deriving (Eq, Show)

type GitUrl = String
type GitRef = String

instance FromJSON Dependencies where
  parseJSON v = case v of
    String _ -> dependenciesFromList . return <$> parseJSON v
    Array _ -> dependenciesFromList <$> parseJSON v
    Object _ -> Dependencies <$> parseJSON v
    _ -> typeMismatch "Array, Object, or String" v
    where
      fromDependency :: Dependency -> (String, DependencyVersion)
      fromDependency (Dependency name version) = (name, version)

      dependenciesFromList :: [Dependency] -> Dependencies
      dependenciesFromList = Dependencies . Map.fromList . map fromDependency

instance FromJSON DependencyVersion where
  parseJSON v = case v of
    Null -> return AnyVersion
    Object _ -> SourceDependency <$> parseJSON v
    String s -> parseVersionRange ("== " ++ input) <|> parseVersionRange input
      where
        input = T.unpack s

    _ -> typeMismatch "Null, Object, or String" v

instance FromJSON SourceDependency where
  parseJSON = withObject "SourceDependency" (\o -> let
    local :: Parser SourceDependency
    local = Local <$> o .: "path"

    git :: Parser SourceDependency
    git = GitRef <$> url <*> ref <*> subdir

    url :: Parser String
    url =
          ((githubBaseUrl ++) <$> o .: "github")
      <|> (o .: "git")
      <|> fail "neither key \"git\" nor key \"github\" present"

    ref :: Parser String
    ref = o .: "ref"

    subdir :: Parser (Maybe FilePath)
    subdir = o .:? "subdir"

    in local <|> git)

data Dependency = Dependency {
  _dependencyName :: String
, _dependencyVersion :: DependencyVersion
} deriving (Eq, Show)

instance FromJSON Dependency where
  parseJSON v = case v of
    String _ -> do
      (name, versionRange) <- parseJSON v >>= parseDependency
      return (Dependency name versionRange)
    Object o -> addSourceDependency o
    _ -> typeMismatch "String or an Object" v
    where
      addSourceDependency o = Dependency <$> name <*> (SourceDependency <$> parseJSON v)
        where
          name :: Parser String
          name = o .: "name"

depPkgName :: D.Dependency -> String
#if MIN_VERSION_Cabal(2,0,0)
depPkgName = D.unPackageName . D.depPkgName
#else
depPkgName (D.Dependency (D.PackageName name) _) = name
#endif

depVerRange :: D.Dependency -> D.VersionRange
#if MIN_VERSION_Cabal(2,0,0)
depVerRange = D.depVerRange
#else
depVerRange (D.Dependency _ versionRange) = versionRange
#endif

parseDependency :: Monad m => String -> m (String, DependencyVersion)
parseDependency = liftM fromCabal . parseCabalDependency
  where
    fromCabal :: D.Dependency -> (String, DependencyVersion)
    fromCabal d = (depPkgName d, dependencyVersionFromCabal $ depVerRange d)

dependencyVersionFromCabal :: D.VersionRange -> DependencyVersion
dependencyVersionFromCabal versionRange
  | D.isAnyVersion versionRange = AnyVersion
  | otherwise = VersionRange . renderStyle style . D.disp $ versionRange
  where
    style = Style OneLineMode 0 0

parseCabalDependency :: Monad m => String -> m D.Dependency
parseCabalDependency = cabalParse "dependency"

parseVersionRange :: Monad m => String -> m DependencyVersion
parseVersionRange = liftM dependencyVersionFromCabal . parseCabalVersionRange

parseCabalVersionRange :: Monad m => String -> m D.VersionRange
parseCabalVersionRange = cabalParse "constraint"

cabalParse :: (Monad m, D.Text a) => String -> String -> m a
cabalParse subject s = case [d | (d, "") <- D.readP_to_S D.parse s] of
  [d] -> return d
  _ -> fail $ unwords ["invalid",  subject, show s]
