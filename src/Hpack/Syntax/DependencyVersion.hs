{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
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

import           Control.Applicative
import qualified Control.Monad.Fail as Fail
import           Data.Maybe
import           Data.Scientific
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HashMap
import           Text.PrettyPrint (renderStyle, Style(..), Mode(..))

import           Distribution.Version (VersionRangeF(..))
import qualified Distribution.Version as D

#if MIN_VERSION_Cabal(3,0,0)
import qualified Distribution.Parsec as D
import qualified Distribution.Pretty as D
#else
import qualified Distribution.Parsec.Class as D
import qualified Distribution.Text as D
#endif

import           Data.Aeson.Config.FromValue

githubBaseUrl :: String
githubBaseUrl = "https://github.com/"

type GitUrl = String
type GitRef = String

data VersionConstraint = AnyVersion | VersionRange String
  deriving (Eq, Show)

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
  deriving (Eq, Show)

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
  deriving (Eq, Show)

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
      | any (`HashMap.member` o) ["path", "git", "github", "ref", "subdir"] = Just <$> (local <|> git)
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

versionConstraintFromCabal :: D.VersionRange -> VersionConstraint
versionConstraintFromCabal range
  | D.isAnyVersion range = AnyVersion
  | otherwise = VersionRange . renderStyle style .
#if MIN_VERSION_Cabal(3,0,0)
      D.pretty
#else
      D.disp
#endif
      $ toPreCabal2VersionRange range
  where
    style = Style OneLineMode 0 0

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
