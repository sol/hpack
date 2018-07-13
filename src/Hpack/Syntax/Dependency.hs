{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
module Hpack.Syntax.Dependency (
  Dependencies(..)
, DependencyVersion(..)
, SourceDependency(..)
, GitRef
, GitUrl
, githubBaseUrl
, scientificToVersion
) where

import qualified Data.Text as T
import           Data.Semigroup (Semigroup(..))
import           Text.PrettyPrint (renderStyle, Style(..), Mode(..))
import           Control.Monad
import           Distribution.Version (VersionRangeF(..))
import qualified Distribution.Compat.ReadP as D
import qualified Distribution.Package as D
import qualified Distribution.Text as D
import qualified Distribution.Version as D
import           Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import           Data.Scientific
import           Control.Applicative
import           GHC.Exts

import           Data.Aeson.Config.FromValue

githubBaseUrl :: String
githubBaseUrl = "https://github.com/"

newtype Dependencies = Dependencies {
  unDependencies :: Map String DependencyVersion
} deriving (Eq, Show, Semigroup, Monoid)

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


instance FromValue DependencyVersion where
  fromValue v = case v of
    Null -> return AnyVersion
    Object _ -> SourceDependency <$> fromValue v
    Number n -> return (scientificToDependencyVersion n)
    String s -> parseVersionRange ("== " ++ input) <|> parseVersionRange input
      where
        input = T.unpack s

    _ -> typeMismatch "Null, Object, Number, or String" v

scientificToDependencyVersion :: Scientific -> DependencyVersion
scientificToDependencyVersion n = VersionRange ("==" ++ version)
  where
    version = scientificToVersion n

scientificToVersion :: Scientific -> String
scientificToVersion n = version
  where
    version = formatScientific Fixed (Just decimalPlaces) n
    decimalPlaces
      | e < 0 = abs e
      | otherwise = 0
    e = base10Exponent n

instance FromValue SourceDependency where
  fromValue = withObject (\o -> let
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
parseCabalVersionRange = fmap toPreCabal2VersionRange . cabalParse "constraint"

cabalParse :: (Monad m, D.Text a) => String -> String -> m a
cabalParse subject s = case [d | (d, "") <- D.readP_to_S D.parse s] of
  [d] -> return d
  _ -> fail $ unwords ["invalid",  subject, show s]


toPreCabal2VersionRange :: D.VersionRange -> D.VersionRange
toPreCabal2VersionRange = D.embedVersionRange . D.cataVersionRange f
  where
    f :: VersionRangeF (VersionRangeF D.VersionRange) -> VersionRangeF D.VersionRange
    f = \ case
      MajorBoundVersionF v -> IntersectVersionRangesF (D.embedVersionRange lower) (D.embedVersionRange upper)
        where
          lower = OrLaterVersionF v
          upper = EarlierVersionF (D.majorUpperBound v)

      AnyVersionF -> AnyVersionF
      ThisVersionF v -> ThisVersionF v
      LaterVersionF v -> LaterVersionF v
      OrLaterVersionF v -> OrLaterVersionF v
      EarlierVersionF v -> EarlierVersionF v
      OrEarlierVersionF v -> OrEarlierVersionF v
      WildcardVersionF v -> WildcardVersionF v
      UnionVersionRangesF a b -> UnionVersionRangesF (D.embedVersionRange a) (D.embedVersionRange b)
      IntersectVersionRangesF a b -> IntersectVersionRangesF (D.embedVersionRange a) (D.embedVersionRange b)
      VersionRangeParensF a -> VersionRangeParensF (D.embedVersionRange a)
