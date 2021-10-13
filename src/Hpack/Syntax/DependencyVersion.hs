{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
module Hpack.Syntax.DependencyVersion (
  githubBaseUrl
, GitRef
, GitUrl

, VersionConstraint(..)
, versionConstraint
, anyVersion
, versionRange

, DependencyVersion(..)
, withDependencyVersion
, dependencyVersion

, SourceDependency(..)
, objectDependency

, versionConstraintFromCabal

, scientificToVersion
, cabalParse
) where

import           Imports

import qualified Control.Monad.Fail as Fail
import           Data.Maybe
import           Data.Scientific
import qualified Data.Text as T
import qualified Data.Aeson.Config.KeyMap as KeyMap
import           Text.PrettyPrint (renderStyle, Style(..), Mode(..))

import qualified Distribution.Version as D

import qualified Distribution.Parsec as D
import qualified Distribution.Pretty as D
import qualified Distribution.Types.VersionRange.Internal as D

import           Data.Aeson.Config.FromValue

githubBaseUrl :: String
githubBaseUrl = "https://github.com/"

type GitUrl = String
type GitRef = String

data VersionConstraint = AnyVersion | VersionRange String
  deriving (Eq, Ord, Show)

instance FromValue VersionConstraint where
  fromValue = versionConstraint

versionConstraint :: Value -> Parser VersionConstraint
versionConstraint v = case v of
  Null -> return AnyVersion
  Number n -> return (numericVersionConstraint n)
  String s -> stringVersionConstraint s
  _ -> typeMismatch "Null, Number, or String" v

anyVersion :: DependencyVersion
anyVersion = DependencyVersion Nothing AnyVersion

versionRange :: String -> DependencyVersion
versionRange = DependencyVersion Nothing . VersionRange

data DependencyVersion = DependencyVersion (Maybe SourceDependency) VersionConstraint
  deriving (Eq, Ord, Show)

withDependencyVersion
  :: (DependencyVersion -> a)
  -> (Object -> DependencyVersion -> Parser a)
  -> Value
  -> Parser a
withDependencyVersion k obj v = case v of
  Null -> return $ k anyVersion
  Object o -> objectDependency o >>= obj o
  Number n -> return $ k (DependencyVersion Nothing $ numericVersionConstraint n)
  String s -> k . DependencyVersion Nothing <$> stringVersionConstraint s
  _ -> typeMismatch "Null, Object, Number, or String" v

dependencyVersion :: Value -> Parser DependencyVersion
dependencyVersion = withDependencyVersion id (const return)

data SourceDependency = GitRef GitUrl GitRef (Maybe FilePath) | Local FilePath
  deriving (Eq, Ord, Show)

objectDependency :: Object -> Parser DependencyVersion
objectDependency o = let
    version :: Parser VersionConstraint
    version = fromMaybe AnyVersion <$> (o .:? "version")

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

    source :: Parser (Maybe SourceDependency)
    source
      | any (`KeyMap.member` o) ["path", "git", "github", "ref", "subdir"] = Just <$> (local <|> git)
      | otherwise = return Nothing

    in DependencyVersion <$> source <*> version

numericVersionConstraint :: Scientific -> VersionConstraint
numericVersionConstraint n = VersionRange ("==" ++ version)
  where
    version = scientificToVersion n

stringVersionConstraint :: Text -> Parser VersionConstraint
stringVersionConstraint s = parseVersionRange ("== " ++ input) <|> parseVersionRange input
  where
    input = T.unpack s

scientificToVersion :: Scientific -> String
scientificToVersion n = version
  where
    version = formatScientific Fixed (Just decimalPlaces) n
    decimalPlaces
      | e < 0 = abs e
      | otherwise = 0
    e = base10Exponent n

parseVersionRange :: Fail.MonadFail m => String -> m VersionConstraint
parseVersionRange = fmap versionConstraintFromCabal . parseCabalVersionRange

parseCabalVersionRange :: Fail.MonadFail m => String -> m D.VersionRange
parseCabalVersionRange = cabalParse "constraint"

cabalParse :: (Fail.MonadFail m, D.Parsec a) => String -> String -> m a
cabalParse subject s = case D.eitherParsec s of
  Right d -> return d
  Left _ ->fail $ unwords ["invalid",  subject, show s]

renderVersionRange :: D.VersionRange -> String
renderVersionRange = \ case
  D.IntersectVersionRanges (D.OrLaterVersion x) (D.EarlierVersion y) | differByOneInLeastPosition (x, y) -> "==" ++ render x ++ ".*"
  v -> render v
  where
    differByOneInLeastPosition = \ case
      (reverse . D.versionNumbers -> x : xs, reverse . D.versionNumbers -> y : ys) -> xs == ys && succ x == y
      _ -> False

render :: D.Pretty a => a -> String
render = renderStyle (Style OneLineMode 0 0) . D.pretty

versionConstraintFromCabal :: D.VersionRange -> VersionConstraint
versionConstraintFromCabal range
  | D.isAnyVersion range = AnyVersion
  | otherwise = VersionRange . renderVersionRange $ toPreCabal2VersionRange range
  where
    toPreCabal2VersionRange :: D.VersionRange -> D.VersionRange
    toPreCabal2VersionRange = D.embedVersionRange . D.cataVersionRange f
      where
        f :: D.VersionRangeF (D.VersionRangeF D.VersionRange) -> D.VersionRangeF D.VersionRange
        f = \ case
          D.MajorBoundVersionF v -> D.IntersectVersionRangesF (D.embedVersionRange lower) (D.embedVersionRange upper)
            where
              lower = D.OrLaterVersionF v
              upper = D.EarlierVersionF (D.majorUpperBound v)

          D.ThisVersionF v -> D.ThisVersionF v
          D.LaterVersionF v -> D.LaterVersionF v
          D.OrLaterVersionF v -> D.OrLaterVersionF v
          D.EarlierVersionF v -> D.EarlierVersionF v
          D.OrEarlierVersionF v -> D.OrEarlierVersionF v
          D.UnionVersionRangesF a b -> D.UnionVersionRangesF (D.embedVersionRange a) (D.embedVersionRange b)
          D.IntersectVersionRangesF a b -> D.IntersectVersionRangesF (D.embedVersionRange a) (D.embedVersionRange b)
#if !MIN_VERSION_Cabal(3,4,0)
          D.WildcardVersionF v -> D.WildcardVersionF v
          D.VersionRangeParensF a -> D.VersionRangeParensF (D.embedVersionRange a)
          D.AnyVersionF -> D.AnyVersionF
#endif
