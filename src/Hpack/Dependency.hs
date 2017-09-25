{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Hpack.Dependency (
  Dependencies(..)
, DependencyVersion(..)
, SourceDependency(..)
, GitRef
, GitUrl
, githubBaseUrl
, parseDependency
#ifdef TEST
, Dependency(..)
#endif
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
import qualified Data.Foldable as Foldable
import qualified Data.HashMap.Lazy as HashMap
import           Control.Arrow ((&&&))

githubBaseUrl :: String
githubBaseUrl = "https://github.com/"

newtype Dependencies = Dependencies {
  unDependencies :: Map String DependencyVersion
} deriving (Eq, Show, Monoid)

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
    Array a -> do
      dependencies <- mapM parseJSON $ Foldable.toList a
      pure . Dependencies . Map.fromList . map (dependencyName &&& dependencyVersion) $ dependencies
    Object o -> do
      dependencies <- forM (HashMap.toList o) $ \ (key, value) ->
        let name = T.unpack key
        in case value of
          Null -> pure (name, AnyVersion)
          Object _ -> do
            source <- parseJSON value
            pure (name, SourceDependency source)
          String s -> pure (name, VersionRange $ T.unpack s)
          _ -> typeMismatch "Null, Object, or String" value
      pure . Dependencies . Map.fromList $ dependencies
    String _ -> do
      Dependency name version <- parseJSON v
      pure . Dependencies $ Map.singleton name version
    _ -> typeMismatch "Array, Object, or String" v

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
  dependencyName :: String
, dependencyVersion :: DependencyVersion
} deriving (Eq, Show)

instance FromJSON Dependency where
  parseJSON v = case v of
    String _ -> do
      (name, versionRange) <- parseJSON v >>= parseDependency
      return (Dependency name $ maybe AnyVersion VersionRange versionRange)
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

parseDependency :: Monad m => String -> m (String, Maybe String)
parseDependency = liftM render . parseCabalDependency
  where
    render :: D.Dependency -> (String, Maybe String)
    render d = (name, range)
      where
        name = depPkgName d
        versionRange = depVerRange d

        range
          | D.isAnyVersion versionRange = Nothing
          | otherwise = Just . renderStyle style . D.disp $ versionRange
          where
            style = Style OneLineMode 0 0

parseCabalDependency :: Monad m => String -> m D.Dependency
parseCabalDependency s = case [d | (d, "") <- D.readP_to_S D.parse s] of
  [d] -> return d
  _ -> fail $ "invalid dependency " ++ show s
