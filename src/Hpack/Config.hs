{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
module Hpack.Config (
-- | /__NOTE:__/ This module is exposed to allow integration of Hpack into
-- other tools.  It is not meant for general use by end users.  The following
-- caveats apply:
--
-- * The API is undocumented, consult the source instead.
--
-- * The exposed types and functions primarily serve Hpack's own needs, not
-- that of a public API.  Breaking changes can happen as Hpack evolves.
--
-- As an Hpack user you either want to use the @hpack@ executable or a build
-- tool that supports Hpack (e.g. @stack@ or @cabal2nix@).

  DecodeOptions(..)
, ProgramName(..)
, defaultDecodeOptions
, packageConfig
, DecodeResult(..)
, readPackageConfig

, renamePackage
, packageDependencies
, package
, section
, Package(..)
, Dependencies(..)
, VersionConstraint(..)
, DependencyVersion(..)
, SourceDependency(..)
, GitRef
, GitUrl
, BuildTool(..)
, SystemBuildTools(..)
, GhcOption
, Verbatim(..)
, VerbatimValue(..)
, verbatimValueToString
, CustomSetup(..)
, Section(..)
, Library(..)
, Executable(..)
, Conditional(..)
, Flag(..)
, SourceRepository(..)
, BuildType(..)
, GhcProfOption
, GhcjsOption
, CppOption
, CcOption
, LdOption
, Path(..)
#ifdef TEST
, renameDependencies
, Empty(..)
, getModules
, pathsModuleFromPackageName
, Cond(..)

, LibrarySection(..)
, fromLibrarySectionInConditional
, formatOrList

, toBuildTool
#endif
) where

import           Control.Applicative
import           Control.Arrow ((>>>), (&&&))
import           Control.Monad
import           Data.Either
import           Data.Bifunctor
import           Data.Bitraversable
import           Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import qualified Data.HashMap.Lazy as HashMap
import           Data.List (nub, (\\), sortBy, intercalate)
import           Data.Maybe
import           Data.Semigroup (Semigroup(..))
import           Data.Ord
import           Data.String
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Scientific (Scientific)
import           System.Directory
import           System.FilePath
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Writer
import           Control.Monad.Trans.Except
import           Control.Monad.IO.Class
import           Data.Version (Version, makeVersion, showVersion)

import           Distribution.Pretty (prettyShow)
import qualified Distribution.SPDX.License as SPDX

import           Data.Aeson.Config.Types
import           Data.Aeson.Config.FromValue hiding (decodeValue)
import qualified Data.Aeson.Config.FromValue as Config

import           Hpack.Syntax.Defaults
import           Hpack.Util hiding (expandGlobs)
import qualified Hpack.Util as Util
import           Hpack.Defaults
import qualified Hpack.Yaml as Yaml
import           Hpack.Syntax.DependencyVersion
import           Hpack.Syntax.Dependencies
import           Hpack.Syntax.BuildTools
import           Hpack.License
import           Hpack.CabalFile (parseVersion)

import qualified Paths_hpack as Hpack (version)

package :: String -> String -> Package
package name version = Package {
    packageName = name
  , packageVersion = version
  , packageSynopsis = Nothing
  , packageDescription = Nothing
  , packageHomepage = Nothing
  , packageBugReports = Nothing
  , packageCategory = Nothing
  , packageStability = Nothing
  , packageAuthor = []
  , packageMaintainer = []
  , packageCopyright = []
  , packageBuildType = Simple
  , packageLicense = Nothing
  , packageLicenseFile = []
  , packageTestedWith = Nothing
  , packageFlags = []
  , packageExtraSourceFiles = []
  , packageExtraDocFiles = []
  , packageDataFiles = []
  , packageDataDir = Nothing
  , packageSourceRepository = Nothing
  , packageCustomSetup = Nothing
  , packageLibrary = Nothing
  , packageInternalLibraries = mempty
  , packageExecutables = mempty
  , packageTests = mempty
  , packageBenchmarks = mempty
  , packageVerbatim = []
  }

renamePackage :: String -> Package -> Package
renamePackage name p@Package{..} = p {
    packageName = name
  , packageExecutables = fmap (renameDependencies packageName name) packageExecutables
  , packageTests = fmap (renameDependencies packageName name) packageTests
  , packageBenchmarks = fmap (renameDependencies packageName name) packageBenchmarks
  }

renameDependencies :: String -> String -> Section a -> Section a
renameDependencies old new sect@Section{..} = sect {sectionDependencies = (Dependencies . Map.fromList . map rename . Map.toList . unDependencies) sectionDependencies, sectionConditionals = map renameConditional sectionConditionals}
  where
    rename dep@(name, version)
      | name == old = (new, version)
      | otherwise = dep

    renameConditional :: Conditional (Section a) -> Conditional (Section a)
    renameConditional (Conditional condition then_ else_) = Conditional condition (renameDependencies old new then_) (renameDependencies old new <$> else_)

packageDependencies :: Package -> [(String, DependencyInfo)]
packageDependencies Package{..} = nub . sortBy (comparing (lexicographically . fst)) $
     (concatMap deps packageExecutables)
  ++ (concatMap deps packageTests)
  ++ (concatMap deps packageBenchmarks)
  ++ maybe [] deps packageLibrary
  where
    deps xs = [(name, info) | (name, info) <- (Map.toList . unDependencies . sectionDependencies) xs]

section :: a -> Section a
section a = Section a [] mempty [] [] [] [] [] [] [] [] [] [] [] [] [] [] [] [] [] [] [] Nothing [] mempty mempty []

packageConfig :: FilePath
packageConfig = "package.yaml"

data CustomSetupSection = CustomSetupSection {
  customSetupSectionDependencies :: Maybe Dependencies
} deriving (Eq, Show, Generic, FromValue)

data LibrarySection = LibrarySection {
  librarySectionExposed :: Maybe Bool
, librarySectionExposedModules :: Maybe (List String)
, librarySectionGeneratedExposedModules :: Maybe (List String)
, librarySectionOtherModules :: Maybe (List String)
, librarySectionGeneratedOtherModules :: Maybe (List String)
, librarySectionReexportedModules :: Maybe (List String)
, librarySectionSignatures :: Maybe (List String)
} deriving (Eq, Show, Generic, FromValue)

instance Monoid LibrarySection where
  mempty = LibrarySection Nothing Nothing Nothing Nothing Nothing Nothing Nothing
  mappend = (<>)

instance Semigroup LibrarySection where
  a <> b = LibrarySection {
      librarySectionExposed = librarySectionExposed b <|> librarySectionExposed a
    , librarySectionExposedModules = librarySectionExposedModules a <> librarySectionExposedModules b
    , librarySectionGeneratedExposedModules = librarySectionGeneratedExposedModules a <> librarySectionGeneratedExposedModules b
    , librarySectionOtherModules = librarySectionOtherModules a <> librarySectionOtherModules b
    , librarySectionGeneratedOtherModules = librarySectionGeneratedOtherModules a <> librarySectionGeneratedOtherModules b
    , librarySectionReexportedModules = librarySectionReexportedModules a <> librarySectionReexportedModules b
    , librarySectionSignatures = librarySectionSignatures a <> librarySectionSignatures b
    }

data ExecutableSection = ExecutableSection {
  executableSectionMain :: Maybe FilePath
, executableSectionOtherModules :: Maybe (List String)
, executableSectionGeneratedOtherModules :: Maybe (List String)
} deriving (Eq, Show, Generic, FromValue)

instance Monoid ExecutableSection where
  mempty = ExecutableSection Nothing Nothing Nothing
  mappend = (<>)

instance Semigroup ExecutableSection where
  a <> b = ExecutableSection {
      executableSectionMain = executableSectionMain b <|> executableSectionMain a
    , executableSectionOtherModules = executableSectionOtherModules a <> executableSectionOtherModules b
    , executableSectionGeneratedOtherModules = executableSectionGeneratedOtherModules a <> executableSectionGeneratedOtherModules b
    }

data VerbatimValue =
    VerbatimString String
  | VerbatimNumber Scientific
  | VerbatimBool Bool
  | VerbatimNull
  deriving (Eq, Show)

instance FromValue VerbatimValue where
  fromValue v = case v of
    String s -> return (VerbatimString $ T.unpack s)
    Number n -> return (VerbatimNumber n)
    Bool b -> return (VerbatimBool b)
    Null -> return VerbatimNull
    Object _ -> err
    Array _ -> err
    where
      err = typeMismatch (formatOrList ["String", "Number", "Bool", "Null"]) v

data Verbatim = VerbatimLiteral String | VerbatimObject (Map String VerbatimValue)
  deriving (Eq, Show)

instance FromValue Verbatim where
  fromValue v = case v of
    String s -> return (VerbatimLiteral $ T.unpack s)
    Object _ -> VerbatimObject <$> fromValue v
    _ -> typeMismatch (formatOrList ["String", "Object"]) v

data CommonOptions cSources cxxSources jsSources a = CommonOptions {
  commonOptionsSourceDirs :: Maybe (List FilePath)
, commonOptionsDependencies :: Maybe Dependencies
, commonOptionsPkgConfigDependencies :: Maybe (List String)
, commonOptionsDefaultExtensions :: Maybe (List String)
, commonOptionsOtherExtensions :: Maybe (List String)
, commonOptionsGhcOptions :: Maybe (List GhcOption)
, commonOptionsGhcProfOptions :: Maybe (List GhcProfOption)
, commonOptionsGhcjsOptions :: Maybe (List GhcjsOption)
, commonOptionsCppOptions :: Maybe (List CppOption)
, commonOptionsCcOptions :: Maybe (List CcOption)
, commonOptionsCSources :: cSources
, commonOptionsCxxOptions :: Maybe (List CxxOption)
, commonOptionsCxxSources :: cxxSources
, commonOptionsJsSources :: jsSources
, commonOptionsExtraLibDirs :: Maybe (List FilePath)
, commonOptionsExtraLibraries :: Maybe (List FilePath)
, commonOptionsExtraFrameworksDirs :: Maybe (List FilePath)
, commonOptionsFrameworks :: Maybe (List String)
, commonOptionsIncludeDirs :: Maybe (List FilePath)
, commonOptionsInstallIncludes :: Maybe (List FilePath)
, commonOptionsLdOptions :: Maybe (List LdOption)
, commonOptionsBuildable :: Maybe Bool
, commonOptionsWhen :: Maybe (List (ConditionalSection cSources cxxSources jsSources a))
, commonOptionsBuildTools :: Maybe BuildTools
, commonOptionsSystemBuildTools :: Maybe SystemBuildTools
, commonOptionsVerbatim :: Maybe (List Verbatim)
} deriving (Functor, Generic)

type ParseCommonOptions = CommonOptions ParseCSources ParseCxxSources ParseJsSources
instance FromValue a => FromValue (ParseCommonOptions a)

instance (Semigroup cSources, Semigroup cxxSources, Semigroup jsSources, Monoid cSources, Monoid cxxSources, Monoid jsSources) => Monoid (CommonOptions cSources cxxSources jsSources a) where
  mempty = CommonOptions {
    commonOptionsSourceDirs = Nothing
  , commonOptionsDependencies = Nothing
  , commonOptionsPkgConfigDependencies = Nothing
  , commonOptionsDefaultExtensions = Nothing
  , commonOptionsOtherExtensions = Nothing
  , commonOptionsGhcOptions = Nothing
  , commonOptionsGhcProfOptions = Nothing
  , commonOptionsGhcjsOptions = Nothing
  , commonOptionsCppOptions = Nothing
  , commonOptionsCcOptions = Nothing
  , commonOptionsCSources = mempty
  , commonOptionsCxxOptions = Nothing
  , commonOptionsCxxSources = mempty
  , commonOptionsJsSources = mempty
  , commonOptionsExtraLibDirs = Nothing
  , commonOptionsExtraLibraries = Nothing
  , commonOptionsExtraFrameworksDirs = Nothing
  , commonOptionsFrameworks = Nothing
  , commonOptionsIncludeDirs = Nothing
  , commonOptionsInstallIncludes = Nothing
  , commonOptionsLdOptions = Nothing
  , commonOptionsBuildable = Nothing
  , commonOptionsWhen = Nothing
  , commonOptionsBuildTools = Nothing
  , commonOptionsSystemBuildTools = Nothing
  , commonOptionsVerbatim = Nothing
  }
  mappend = (<>)

instance (Semigroup cSources, Semigroup cxxSources, Semigroup jsSources) => Semigroup (CommonOptions cSources cxxSources jsSources a) where
  a <> b = CommonOptions {
    commonOptionsSourceDirs = commonOptionsSourceDirs a <> commonOptionsSourceDirs b
  , commonOptionsDependencies = commonOptionsDependencies b <> commonOptionsDependencies a
  , commonOptionsPkgConfigDependencies = commonOptionsPkgConfigDependencies a <> commonOptionsPkgConfigDependencies b
  , commonOptionsDefaultExtensions = commonOptionsDefaultExtensions a <> commonOptionsDefaultExtensions b
  , commonOptionsOtherExtensions = commonOptionsOtherExtensions a <> commonOptionsOtherExtensions b
  , commonOptionsGhcOptions = commonOptionsGhcOptions a <> commonOptionsGhcOptions b
  , commonOptionsGhcProfOptions = commonOptionsGhcProfOptions a <> commonOptionsGhcProfOptions b
  , commonOptionsGhcjsOptions = commonOptionsGhcjsOptions a <> commonOptionsGhcjsOptions b
  , commonOptionsCppOptions = commonOptionsCppOptions a <> commonOptionsCppOptions b
  , commonOptionsCcOptions = commonOptionsCcOptions a <> commonOptionsCcOptions b
  , commonOptionsCSources = commonOptionsCSources a <> commonOptionsCSources b
  , commonOptionsCxxOptions = commonOptionsCxxOptions a <> commonOptionsCxxOptions b
  , commonOptionsCxxSources = commonOptionsCxxSources a <> commonOptionsCxxSources b
  , commonOptionsJsSources = commonOptionsJsSources a <> commonOptionsJsSources b
  , commonOptionsExtraLibDirs = commonOptionsExtraLibDirs a <> commonOptionsExtraLibDirs b
  , commonOptionsExtraLibraries = commonOptionsExtraLibraries a <> commonOptionsExtraLibraries b
  , commonOptionsExtraFrameworksDirs = commonOptionsExtraFrameworksDirs a <> commonOptionsExtraFrameworksDirs b
  , commonOptionsFrameworks = commonOptionsFrameworks a <> commonOptionsFrameworks b
  , commonOptionsIncludeDirs = commonOptionsIncludeDirs a <> commonOptionsIncludeDirs b
  , commonOptionsInstallIncludes = commonOptionsInstallIncludes a <> commonOptionsInstallIncludes b
  , commonOptionsLdOptions = commonOptionsLdOptions a <> commonOptionsLdOptions b
  , commonOptionsBuildable = commonOptionsBuildable b <|> commonOptionsBuildable a
  , commonOptionsWhen = commonOptionsWhen a <> commonOptionsWhen b
  , commonOptionsBuildTools = commonOptionsBuildTools a <> commonOptionsBuildTools b
  , commonOptionsSystemBuildTools = commonOptionsSystemBuildTools b <> commonOptionsSystemBuildTools a
  , commonOptionsVerbatim = commonOptionsVerbatim a <> commonOptionsVerbatim b
  }

type ParseCSources = Maybe (List FilePath)
type ParseCxxSources = Maybe (List FilePath)
type ParseJsSources = Maybe (List FilePath)

type CSources = [Path]
type CxxSources = [Path]
type JsSources = [Path]

type WithCommonOptions cSources cxxSources jsSources a = Product (CommonOptions cSources cxxSources jsSources a) a

data Traverse m cSources cSources_ cxxSources cxxSources_ jsSources jsSources_ = Traverse {
  traverseCSources :: cSources -> m cSources_
, traverseCxxSources :: cxxSources -> m cxxSources_
, traverseJsSources :: jsSources -> m jsSources_
}

type Traversal t = forall m cSources cSources_ cxxSources cxxSources_ jsSources jsSources_. Monad m
  => Traverse m cSources cSources_ cxxSources cxxSources_ jsSources jsSources_
  -> t cSources cxxSources jsSources
  -> m (t cSources_ cxxSources_ jsSources_)

type Traversal_ t = forall m cSources cSources_ cxxSources cxxSources_ jsSources jsSources_ a. Monad m
  => Traverse m cSources cSources_ cxxSources cxxSources_ jsSources jsSources_
  -> t cSources cxxSources jsSources a
  -> m (t cSources_ cxxSources_ jsSources_ a)

traverseCommonOptions :: Traversal_ CommonOptions
traverseCommonOptions t@Traverse{..} c@CommonOptions{..} = do
  cSources <- traverseCSources commonOptionsCSources
  cxxSources <- traverseCxxSources commonOptionsCxxSources
  jsSources <- traverseJsSources commonOptionsJsSources
  xs <- traverse (traverse (traverseConditionalSection t)) commonOptionsWhen
  return c {
      commonOptionsCSources = cSources
    , commonOptionsCxxSources = cxxSources
    , commonOptionsJsSources = jsSources
    , commonOptionsWhen = xs
    }

traverseConditionalSection :: Traversal_ ConditionalSection
traverseConditionalSection t@Traverse{..} = \ case
  ThenElseConditional c -> ThenElseConditional <$> bitraverse (traverseThenElse t) return c
  FlatConditional c -> FlatConditional <$> bitraverse (traverseWithCommonOptions t) return c

traverseThenElse :: Traversal_ ThenElse
traverseThenElse t@Traverse{..} c@ThenElse{..} = do
  then_ <- traverseWithCommonOptions t thenElseThen
  else_ <- traverseWithCommonOptions t thenElseElse
  return c{thenElseThen = then_, thenElseElse = else_}

traverseWithCommonOptions :: Traversal_ WithCommonOptions
traverseWithCommonOptions t = bitraverse (traverseCommonOptions t) return

data ConditionalSection cSources cxxSources jsSources a =
    ThenElseConditional (Product (ThenElse cSources cxxSources jsSources a) Condition)
  | FlatConditional (Product (WithCommonOptions cSources cxxSources jsSources a) Condition)

instance Functor (ConditionalSection cSources cxxSources jsSources) where
  fmap f = \ case
    ThenElseConditional c -> ThenElseConditional (first (fmap f) c)
    FlatConditional c -> FlatConditional (first (bimap (fmap f) f) c)

type ParseConditionalSection = ConditionalSection ParseCSources ParseCxxSources ParseJsSources

instance FromValue a => FromValue (ParseConditionalSection a) where
  fromValue v
    | hasKey "then" v || hasKey "else" v = ThenElseConditional <$> fromValue v
    | otherwise = FlatConditional <$> fromValue v

hasKey :: Text -> Value -> Bool
hasKey key (Object o) = HashMap.member key o
hasKey _ _ = False

newtype Condition = Condition {
  _conditionCondition :: Cond
} deriving (Eq, Show, Generic, FromValue)

newtype Cond = Cond String
  deriving (Eq, Show)

instance FromValue Cond where
  fromValue v = case v of
    String s -> return (Cond $ T.unpack s)
    Bool True -> return (Cond "true")
    Bool False -> return (Cond "false")
    _ -> typeMismatch "Boolean or String" v

data ThenElse cSources cxxSources jsSources a = ThenElse {
  thenElseThen :: WithCommonOptions cSources cxxSources jsSources a
, thenElseElse :: WithCommonOptions cSources cxxSources jsSources a
} deriving Generic

instance Functor (ThenElse cSources cxxSources jsSources) where
  fmap f c@ThenElse{..} = c{thenElseThen = map_ thenElseThen, thenElseElse = map_ thenElseElse}
    where
      map_ = bimap (fmap f) f

type ParseThenElse = ThenElse ParseCSources ParseCxxSources ParseJsSources

instance FromValue a => FromValue (ParseThenElse a)

data Empty = Empty
  deriving (Eq, Show)

instance Monoid Empty where
  mempty = Empty
  mappend = (<>)

instance Semigroup Empty where
  Empty <> Empty = Empty

instance FromValue Empty where
  fromValue _ = return Empty

data BuildType =
    Simple
  | Configure
  | Make
  | Custom
  deriving (Eq, Show, Generic, Enum, Bounded)

instance FromValue BuildType where
  fromValue = withText $ \ (T.unpack -> t) -> do
    maybe err return (lookup t options)
    where
      err = fail ("expected one of " ++ formatOrList buildTypesAsString)
      buildTypes = [minBound .. maxBound]
      buildTypesAsString = map show buildTypes
      options = zip buildTypesAsString buildTypes

formatOrList :: [String] -> String
formatOrList xs = case reverse xs of
  [] -> ""
  x : [] -> x
  y : x : [] -> x ++ " or " ++ y
  x : ys@(_:_:_) -> intercalate ", " . reverse $ ("or " ++ x) : ys

type SectionConfigWithDefaluts cSources cxxSources jsSources a = Product DefaultsConfig (WithCommonOptions cSources cxxSources jsSources a)

type PackageConfigWithDefaults cSources cxxSources jsSources = PackageConfig_
  (SectionConfigWithDefaluts cSources cxxSources jsSources LibrarySection)
  (SectionConfigWithDefaluts cSources cxxSources jsSources ExecutableSection)

type PackageConfig cSources cxxSources jsSources = PackageConfig_
  (WithCommonOptions cSources cxxSources jsSources LibrarySection)
  (WithCommonOptions cSources cxxSources jsSources ExecutableSection)

data PackageVersion = PackageVersion {unPackageVersion :: String}

instance FromValue PackageVersion where
  fromValue v = PackageVersion <$> case v of
    Number n -> return (scientificToVersion n)
    String s -> return (T.unpack s)
    _ -> typeMismatch "Number or String" v

data PackageConfig_ library executable = PackageConfig {
  packageConfigName :: Maybe String
, packageConfigVersion :: Maybe PackageVersion
, packageConfigSynopsis :: Maybe String
, packageConfigDescription :: Maybe String
, packageConfigHomepage :: Maybe (Maybe String)
, packageConfigBugReports :: Maybe (Maybe String)
, packageConfigCategory :: Maybe String
, packageConfigStability :: Maybe String
, packageConfigAuthor :: Maybe (List String)
, packageConfigMaintainer :: Maybe (Maybe (List String))
, packageConfigCopyright :: Maybe (List String)
, packageConfigBuildType :: Maybe BuildType
, packageConfigLicense :: Maybe (Maybe String)
, packageConfigLicenseFile :: Maybe (List String)
, packageConfigTestedWith :: Maybe String
, packageConfigFlags :: Maybe (Map String FlagSection)
, packageConfigExtraSourceFiles :: Maybe (List FilePath)
, packageConfigExtraDocFiles :: Maybe (List FilePath)
, packageConfigDataFiles :: Maybe (List FilePath)
, packageConfigDataDir :: Maybe FilePath
, packageConfigGithub :: Maybe Text
, packageConfigGit :: Maybe String
, packageConfigCustomSetup :: Maybe CustomSetupSection
, packageConfigLibrary :: Maybe library
, packageConfigInternalLibraries :: Maybe (Map String library)
, packageConfigExecutable :: Maybe executable
, packageConfigExecutables :: Maybe (Map String executable)
, packageConfigTests :: Maybe (Map String executable)
, packageConfigBenchmarks :: Maybe (Map String executable)
} deriving Generic

data DefaultsConfig = DefaultsConfig {
  defaultsConfigDefaults :: Maybe (List Defaults)
} deriving (Generic, FromValue)

traversePackageConfig :: Traversal PackageConfig
traversePackageConfig t@Traverse{..} p@PackageConfig{..} = do
  library <- traverse (traverseWithCommonOptions t) packageConfigLibrary
  internalLibraries <- traverseNamedConfigs t packageConfigInternalLibraries
  executable <- traverse (traverseWithCommonOptions t) packageConfigExecutable
  executables <- traverseNamedConfigs t packageConfigExecutables
  tests <- traverseNamedConfigs t packageConfigTests
  benchmarks <- traverseNamedConfigs t packageConfigBenchmarks
  return p {
      packageConfigLibrary = library
    , packageConfigInternalLibraries = internalLibraries
    , packageConfigExecutable = executable
    , packageConfigExecutables = executables
    , packageConfigTests = tests
    , packageConfigBenchmarks = benchmarks
    }
  where
    traverseNamedConfigs = traverse . traverse . traverseWithCommonOptions

type ParsePackageConfig = PackageConfigWithDefaults ParseCSources ParseCxxSources ParseJsSources

instance FromValue ParsePackageConfig

type Warnings m = WriterT [String] m
type Errors = ExceptT String

decodeYaml :: FromValue a => ProgramName -> FilePath -> Warnings (Errors IO) a
decodeYaml programName file = do
  (warnings, a) <- lift (ExceptT $ Yaml.decodeYaml file)
  tell warnings
  decodeValue programName file a

data DecodeOptions = DecodeOptions {
  decodeOptionsProgramName :: ProgramName
, decodeOptionsTarget :: FilePath
, decodeOptionsUserDataDir :: Maybe FilePath
, decodeOptionsDecode :: FilePath -> IO (Either String ([String], Value))
}

newtype ProgramName = ProgramName String
  deriving (Eq, Show)

instance IsString ProgramName where
  fromString = ProgramName

defaultDecodeOptions :: DecodeOptions
defaultDecodeOptions = DecodeOptions "hpack" packageConfig Nothing Yaml.decodeYaml

data DecodeResult = DecodeResult {
  decodeResultPackage :: Package
, decodeResultCabalVersion :: String
, decodeResultCabalFile :: FilePath
, decodeResultWarnings :: [String]
} deriving (Eq, Show)

readPackageConfig :: DecodeOptions -> IO (Either String DecodeResult)
readPackageConfig (DecodeOptions programName file mUserDataDir readValue) = runExceptT $ fmap addCabalFile . runWriterT $ do
  (warnings, value) <- lift . ExceptT $ readValue file
  tell warnings
  config <- decodeValue programName file value
  dir <- liftIO $ takeDirectory <$> canonicalizePath file
  userDataDir <- liftIO $ maybe (getAppUserDataDirectory "hpack") return mUserDataDir
  toPackage programName userDataDir dir config
  where
    addCabalFile :: ((Package, String), [String]) -> DecodeResult
    addCabalFile ((pkg, cabalVersion), warnings) = DecodeResult pkg cabalVersion (takeDirectory_ file </> (packageName pkg ++ ".cabal")) warnings

    takeDirectory_ :: FilePath -> FilePath
    takeDirectory_ p
      | takeFileName p == p = ""
      | otherwise = takeDirectory p

deleteVerbatimField :: String -> [Verbatim] -> [Verbatim]
deleteVerbatimField name = map $ \ case
  literal@VerbatimLiteral {} -> literal
  VerbatimObject o -> VerbatimObject (Map.delete name o)

verbatimValueToString :: VerbatimValue -> String
verbatimValueToString = \ case
  VerbatimString s -> s
  VerbatimNumber n -> scientificToVersion n
  VerbatimBool b -> show b
  VerbatimNull -> ""

determineCabalVersion :: Maybe (License SPDX.License) -> Package -> (Package, String)
determineCabalVersion inferredLicense pkg@Package{..} = (
    pkg {
        packageVerbatim = deleteVerbatimField "cabal-version" packageVerbatim
      , packageLicense = formatLicense <$> license
      }
  , "cabal-version: " ++ fromMaybe inferredCabalVersion verbatimCabalVersion ++ "\n\n"
  )
  where
    license = fmap prettyShow <$> (parsedLicense <|> inferredLicense)

    parsedLicense = parseLicense <$> packageLicense

    formatLicense = \ case
      MustSPDX spdx -> spdx
      CanSPDX _ spdx | version >= makeVersion [2,2] -> spdx
      CanSPDX cabal _ -> prettyShow cabal
      DontTouch original -> original

    mustSPDX :: Bool
    mustSPDX = maybe False f license
      where
        f = \case
          DontTouch _ -> False
          CanSPDX _ _ -> False
          MustSPDX _ -> True

    verbatimCabalVersion :: Maybe String
    verbatimCabalVersion = listToMaybe (mapMaybe f packageVerbatim)
      where
        f :: Verbatim -> Maybe String
        f = \ case
          VerbatimLiteral _ -> Nothing
          VerbatimObject o -> case Map.lookup "cabal-version" o of
            Just v -> Just (verbatimValueToString v)
            Nothing -> Nothing

    inferredCabalVersion :: String
    inferredCabalVersion = showVersion version

    version = fromMaybe (makeVersion [1,12]) $ maximum [
        packageCabalVersion
      , packageLibrary >>= libraryCabalVersion
      , internalLibsCabalVersion packageInternalLibraries
      , executablesCabalVersion packageExecutables
      , executablesCabalVersion packageTests
      , executablesCabalVersion packageBenchmarks
      ]

    packageCabalVersion :: Maybe Version
    packageCabalVersion = maximum [
        Nothing
      , makeVersion [2,2] <$ guard mustSPDX
      , makeVersion [1,24] <$ packageCustomSetup
      , makeVersion [1,18] <$ guard (not (null packageExtraDocFiles))
      ]

    libraryCabalVersion :: Section Library -> Maybe Version
    libraryCabalVersion sect = maximum [
        makeVersion [1,22] <$ guard hasReexportedModules
      , makeVersion [2,0]  <$ guard hasSignatures
      , makeVersion [2,0] <$ guard hasGeneratedModules
      , sectionCabalVersion sect
      ]
      where
        hasReexportedModules = any (not . null . libraryReexportedModules) sect
        hasSignatures = any (not . null . librarySignatures) sect
        hasGeneratedModules = any (not . null . libraryGeneratedModules) sect

    internalLibsCabalVersion :: Map String (Section Library) -> Maybe Version
    internalLibsCabalVersion internalLibraries
      | Map.null internalLibraries = Nothing
      | otherwise = foldr max (Just $ makeVersion [2,0]) versions
      where
        versions = libraryCabalVersion <$> Map.elems internalLibraries

    executablesCabalVersion :: Map String (Section Executable) -> Maybe Version
    executablesCabalVersion = foldr max Nothing . map executableCabalVersion . Map.elems

    executableCabalVersion :: Section Executable -> Maybe Version
    executableCabalVersion sect = maximum [
        makeVersion [2,0] <$ guard (executableHasGeneratedModules sect)
      , sectionCabalVersion sect
      ]

    executableHasGeneratedModules :: Section Executable -> Bool
    executableHasGeneratedModules = any (not . null . executableGeneratedModules)

    sectionCabalVersion :: Section a -> Maybe Version
    sectionCabalVersion sect = maximum $ [
        makeVersion [2,2] <$ guard (sectionSatisfies (not . null . sectionCxxSources) sect)
      , makeVersion [2,2] <$ guard (sectionSatisfies (not . null . sectionCxxOptions) sect)
      , makeVersion [2,0] <$ guard (sectionSatisfies (any hasMixins . unDependencies . sectionDependencies) sect)
      ] ++ map versionFromSystemBuildTool systemBuildTools
      where
        versionFromSystemBuildTool name
          | name `elem` known_1_10 = Nothing
          | name `elem` known_1_14 = Just (makeVersion [1,14])
          | name `elem` known_1_22 = Just (makeVersion [1,22])
          | otherwise = Just (makeVersion [2,0])

        known_1_10 = [
            "ghc"
          , "ghc-pkg"
          , "hugs"
          , "ffihugs"
          , "nhc98"
          , "hmake"
          , "jhc"
          , "lhc"
          , "lhc-pkg"
          , "uhc"
          , "gcc"
          , "ranlib"
          , "ar"
          , "strip"
          , "ld"
          , "tar"
          , "pkg-config"
          ] \\ [
          -- Support for these build tools has been removed from Cabal at some point
            "hugs"
          , "ffihugs"
          , "nhc98"
          , "ranlib"
          , "lhc"
          , "lhc-pkg"
          ]
        known_1_14 = [
            "hpc"
          ]
        known_1_22 = [
            "ghcjs"
          , "ghcjs-pkg"
          -- , "haskell-suite" // not a real build tool
          -- , "haskell-suite-pkg" // not a real build tool
          ]

        systemBuildTools :: [String]
        systemBuildTools = Map.keys $ unSystemBuildTools $ sectionAll sectionSystemBuildTools sect

    sectionSatisfies :: (Section a -> Bool) -> Section a -> Bool
    sectionSatisfies p sect = or [
        p sect
      , any (any (sectionSatisfies p)) (sectionConditionals sect)
      ]
    sectionAll :: (Semigroup b, Monoid b) => (Section a -> b) -> Section a -> b
    sectionAll f sect = f sect <> foldMap (foldMap $ sectionAll f) (sectionConditionals sect)

    hasMixins :: DependencyInfo -> Bool
    hasMixins (DependencyInfo mixins _) = not (null mixins)

decodeValue :: FromValue a => ProgramName -> FilePath -> Value -> Warnings (Errors IO) a
decodeValue (ProgramName programName) file value = do
  (r, unknown) <- lift . ExceptT . return $ first (prefix ++) (Config.decodeValue value)
  case r of
    UnsupportedSpecVersion v -> do
      lift $ throwE ("The file " ++ file ++ " requires version " ++ showVersion v ++ " of the Hpack package specification, however this version of " ++ programName ++ " only supports versions up to " ++ showVersion Hpack.version ++ ". Upgrading to the latest version of " ++ programName ++ " may resolve this issue.")
    SupportedSpecVersion a -> do
      tell (map formatUnknownField unknown)
      return a
  where
    prefix = file ++ ": "
    formatUnknownField name = prefix ++ "Ignoring unrecognized field " ++ name

data CheckSpecVersion a = SupportedSpecVersion a | UnsupportedSpecVersion Version

instance FromValue a => FromValue (CheckSpecVersion a) where
  fromValue = withObject $ \ o -> o .:? "spec-version" >>= \ case
    Just (ParseSpecVersion v) | Hpack.version < v -> return $ UnsupportedSpecVersion v
    _ -> SupportedSpecVersion <$> fromValue (Object o)

newtype ParseSpecVersion = ParseSpecVersion Version

instance FromValue ParseSpecVersion where
  fromValue value = do
    s <- case value of
      Number n -> return (scientificToVersion n)
      String s -> return (T.unpack s)
      _ -> typeMismatch "Number or String" value
    case parseVersion s of
      Just v -> return (ParseSpecVersion v)
      Nothing -> fail ("invalid value " ++ show s)

data Package = Package {
  packageName :: String
, packageVersion :: String
, packageSynopsis :: Maybe String
, packageDescription :: Maybe String
, packageHomepage :: Maybe String
, packageBugReports :: Maybe String
, packageCategory :: Maybe String
, packageStability :: Maybe String
, packageAuthor :: [String]
, packageMaintainer :: [String]
, packageCopyright :: [String]
, packageBuildType :: BuildType
, packageLicense :: Maybe String
, packageLicenseFile :: [FilePath]
, packageTestedWith :: Maybe String
, packageFlags :: [Flag]
, packageExtraSourceFiles :: [Path]
, packageExtraDocFiles :: [Path]
, packageDataFiles :: [Path]
, packageDataDir :: Maybe FilePath
, packageSourceRepository :: Maybe SourceRepository
, packageCustomSetup :: Maybe CustomSetup
, packageLibrary :: Maybe (Section Library)
, packageInternalLibraries :: Map String (Section Library)
, packageExecutables :: Map String (Section Executable)
, packageTests :: Map String (Section Executable)
, packageBenchmarks :: Map String (Section Executable)
, packageVerbatim :: [Verbatim]
} deriving (Eq, Show)

data CustomSetup = CustomSetup {
  customSetupDependencies :: Dependencies
} deriving (Eq, Show)

data Library = Library {
  libraryExposed :: Maybe Bool
, libraryExposedModules :: [String]
, libraryOtherModules :: [String]
, libraryGeneratedModules :: [String]
, libraryReexportedModules :: [String]
, librarySignatures :: [String]
} deriving (Eq, Show)

data Executable = Executable {
  executableMain :: Maybe FilePath
, executableOtherModules :: [String]
, executableGeneratedModules :: [String]
} deriving (Eq, Show)

data BuildTool = BuildTool String String | LocalBuildTool String
  deriving (Show, Eq, Ord)

data Section a = Section {
  sectionData :: a
, sectionSourceDirs :: [FilePath]
, sectionDependencies :: Dependencies
, sectionPkgConfigDependencies :: [String]
, sectionDefaultExtensions :: [String]
, sectionOtherExtensions :: [String]
, sectionGhcOptions :: [GhcOption]
, sectionGhcProfOptions :: [GhcProfOption]
, sectionGhcjsOptions :: [GhcjsOption]
, sectionCppOptions :: [CppOption]
, sectionCcOptions :: [CcOption]
, sectionCSources :: [Path]
, sectionCxxOptions :: [CxxOption]
, sectionCxxSources :: [Path]
, sectionJsSources :: [Path]
, sectionExtraLibDirs :: [FilePath]
, sectionExtraLibraries :: [FilePath]
, sectionExtraFrameworksDirs :: [FilePath]
, sectionFrameworks :: [FilePath]
, sectionIncludeDirs :: [FilePath]
, sectionInstallIncludes :: [FilePath]
, sectionLdOptions :: [LdOption]
, sectionBuildable :: Maybe Bool
, sectionConditionals :: [Conditional (Section a)]
, sectionBuildTools :: Map BuildTool DependencyVersion
, sectionSystemBuildTools :: SystemBuildTools
, sectionVerbatim :: [Verbatim]
} deriving (Eq, Show, Functor, Foldable, Traversable)

data Conditional a = Conditional {
  conditionalCondition :: String
, conditionalThen :: a
, conditionalElse :: Maybe a
} deriving (Eq, Show, Functor, Foldable, Traversable)

data FlagSection = FlagSection {
  _flagSectionDescription :: Maybe String
, _flagSectionManual :: Bool
, _flagSectionDefault :: Bool
} deriving (Eq, Show, Generic, FromValue)

data Flag = Flag {
  flagName :: String
, flagDescription :: Maybe String
, flagManual :: Bool
, flagDefault :: Bool
} deriving (Eq, Show)

toFlag :: (String, FlagSection) -> Flag
toFlag (name, FlagSection description manual def) = Flag name description manual def

data SourceRepository = SourceRepository {
  sourceRepositoryUrl :: String
, sourceRepositorySubdir :: Maybe String
} deriving (Eq, Show)

type Config cSources cxxSources jsSources =
  Product (CommonOptions cSources cxxSources jsSources Empty) (PackageConfig cSources cxxSources jsSources)

traverseConfig :: Traversal Config
traverseConfig t = bitraverse (traverseCommonOptions t) (traversePackageConfig t)

type ConfigWithDefaults = Product
  (CommonOptionsWithDefaults Empty)
  (PackageConfigWithDefaults ParseCSources ParseCxxSources ParseJsSources)

type CommonOptionsWithDefaults a = Product DefaultsConfig (CommonOptions ParseCSources ParseCxxSources ParseJsSources a)
type WithCommonOptionsWithDefaults a = Product DefaultsConfig (WithCommonOptions ParseCSources ParseCxxSources ParseJsSources a)

toPackage :: ProgramName -> FilePath -> FilePath -> ConfigWithDefaults -> Warnings (Errors IO) (Package, String)
toPackage programName userDataDir dir =
      expandDefaultsInConfig programName userDataDir dir
  >=> traverseConfig (expandForeignSources dir)
  >=> toPackage_ dir

expandDefaultsInConfig
  :: ProgramName
  -> FilePath
  -> FilePath
  -> ConfigWithDefaults
  -> Warnings (Errors IO) (Config ParseCSources ParseCxxSources ParseJsSources)
expandDefaultsInConfig programName userDataDir dir = bitraverse (expandGlobalDefaults programName userDataDir dir) (expandSectionDefaults programName userDataDir dir)

expandGlobalDefaults
  :: ProgramName
  -> FilePath
  -> FilePath
  -> CommonOptionsWithDefaults Empty
  -> Warnings (Errors IO) (CommonOptions ParseCSources ParseCxxSources ParseJsSources Empty)
expandGlobalDefaults programName userDataDir dir = do
  fmap (`Product` Empty) >>> expandDefaults programName userDataDir dir >=> \ (Product c Empty) -> return c

expandSectionDefaults
  :: ProgramName
  -> FilePath
  -> FilePath
  -> PackageConfigWithDefaults ParseCSources ParseCxxSources ParseJsSources
  -> Warnings (Errors IO) (PackageConfig ParseCSources ParseCxxSources ParseJsSources)
expandSectionDefaults programName userDataDir dir p@PackageConfig{..} = do
  library <- traverse (expandDefaults programName userDataDir dir) packageConfigLibrary
  internalLibraries <- traverse (traverse (expandDefaults programName userDataDir dir)) packageConfigInternalLibraries
  executable <- traverse (expandDefaults programName userDataDir dir) packageConfigExecutable
  executables <- traverse (traverse (expandDefaults programName userDataDir dir)) packageConfigExecutables
  tests <- traverse (traverse (expandDefaults programName userDataDir dir)) packageConfigTests
  benchmarks <- traverse (traverse (expandDefaults programName userDataDir dir)) packageConfigBenchmarks
  return p{
      packageConfigLibrary = library
    , packageConfigInternalLibraries = internalLibraries
    , packageConfigExecutable = executable
    , packageConfigExecutables = executables
    , packageConfigTests = tests
    , packageConfigBenchmarks = benchmarks
    }

expandDefaults
  :: (FromValue a, Semigroup a, Monoid a)
  => ProgramName
  -> FilePath
  -> FilePath
  -> WithCommonOptionsWithDefaults a
  -> Warnings (Errors IO) (WithCommonOptions ParseCSources ParseCxxSources ParseJsSources a)
expandDefaults programName userDataDir = expand []
  where
    expand :: (FromValue a, Semigroup a, Monoid a) =>
         [FilePath]
      -> FilePath
      -> WithCommonOptionsWithDefaults a
      -> Warnings (Errors IO) (WithCommonOptions ParseCSources ParseCxxSources ParseJsSources a)
    expand seen dir (Product DefaultsConfig{..} c) = do
      d <- mconcat <$> mapM (get seen dir) (fromMaybeList defaultsConfigDefaults)
      return (d <> c)

    get :: forall a. (FromValue a, Semigroup a, Monoid a) =>
         [FilePath]
      -> FilePath
      -> Defaults
      -> Warnings (Errors IO) (WithCommonOptions ParseCSources ParseCxxSources ParseJsSources a)
    get seen dir defaults = do
      file <- lift $ ExceptT (ensure userDataDir dir defaults)
      seen_ <- lift (checkCycle seen file)
      let dir_ = takeDirectory file
      decodeYaml programName file >>= expand seen_ dir_

    checkCycle :: [FilePath] -> FilePath -> Errors IO [FilePath]
    checkCycle seen file = do
      canonic <- liftIO $ canonicalizePath file
      let seen_ = canonic : seen
      when (canonic `elem` seen) $ do
        throwE ("cycle in defaults (" ++ intercalate " -> " (reverse seen_) ++ ")")
      return seen_

toExecutableMap :: Monad m => String -> Maybe (Map String a) -> Maybe a -> Warnings m (Maybe (Map String a))
toExecutableMap name executables mExecutable = do
  case mExecutable of
    Just executable -> do
      when (isJust executables) $ do
        tell ["Ignoring field \"executables\" in favor of \"executable\""]
      return $ Just (Map.fromList [(name, executable)])
    Nothing -> return executables

type GlobalOptions = CommonOptions CSources CxxSources JsSources Empty

toPackage_ :: MonadIO m => FilePath -> Product GlobalOptions (PackageConfig CSources CxxSources JsSources) -> Warnings m (Package, String)
toPackage_ dir (Product g PackageConfig{..}) = do
  executableMap <- toExecutableMap packageName_ packageConfigExecutables packageConfigExecutable
  let
    globalVerbatim = commonOptionsVerbatim g
    globalOptions = g {commonOptionsVerbatim = Nothing}

    executableNames = maybe [] Map.keys executableMap

    toSect :: (Monad m, Monoid a) => WithCommonOptions CSources CxxSources JsSources a -> Warnings m (Section a)
    toSect = toSection packageName_ executableNames . first ((mempty <$ globalOptions) <>)

    toLib = toSect >=> liftIO . toLibrary dir packageName_
    toExecutables = maybe (return mempty) (traverse $ toSect >=> liftIO . toExecutable dir packageName_)

  mLibrary <- traverse toLib packageConfigLibrary
  internalLibraries <- maybe (return mempty) (traverse toLib) packageConfigInternalLibraries

  executables <- toExecutables executableMap
  tests <- toExecutables packageConfigTests
  benchmarks <- toExecutables packageConfigBenchmarks

  licenseFileExists <- liftIO $ doesFileExist (dir </> "LICENSE")

  missingSourceDirs <- liftIO $ nub . sort <$> filterM (fmap not <$> doesDirectoryExist . (dir </>)) (
       maybe [] sectionSourceDirs mLibrary
    ++ concatMap sectionSourceDirs internalLibraries
    ++ concatMap sectionSourceDirs executables
    ++ concatMap sectionSourceDirs tests
    ++ concatMap sectionSourceDirs benchmarks
    )

  extraSourceFiles <- expandGlobs "extra-source-files" dir (fromMaybeList packageConfigExtraSourceFiles)
  extraDocFiles <- expandGlobs "extra-doc-files" dir (fromMaybeList packageConfigExtraDocFiles)

  let dataBaseDir = maybe dir (dir </>) packageConfigDataDir

  dataFiles <- expandGlobs "data-files" dataBaseDir (fromMaybeList packageConfigDataFiles)

  let
    licenseFiles :: [String]
    licenseFiles = fromMaybeList $ packageConfigLicenseFile <|> do
      guard licenseFileExists
      Just (List ["LICENSE"])

  inferredLicense <- case (packageConfigLicense, licenseFiles) of
    (Nothing, [file]) -> do
      input <- liftIO (tryReadFile (dir </> file))
      case input >>= inferLicense of
        Nothing -> do
          tell ["Inferring license from file " ++ file ++ " failed!"]
          return Nothing
        license -> return license
    _ -> return Nothing

  let defaultBuildType :: BuildType
      defaultBuildType = maybe Simple (const Custom) mCustomSetup

      pkg = Package {
        packageName = packageName_
      , packageVersion = maybe "0.0.0" unPackageVersion packageConfigVersion
      , packageSynopsis = packageConfigSynopsis
      , packageDescription = packageConfigDescription
      , packageHomepage = homepage
      , packageBugReports = bugReports
      , packageCategory = packageConfigCategory
      , packageStability = packageConfigStability
      , packageAuthor = fromMaybeList packageConfigAuthor
      , packageMaintainer = fromMaybeList maintainer
      , packageCopyright = fromMaybeList packageConfigCopyright
      , packageBuildType = fromMaybe defaultBuildType packageConfigBuildType
      , packageLicense = join packageConfigLicense
      , packageLicenseFile = licenseFiles
      , packageTestedWith = packageConfigTestedWith
      , packageFlags = flags
      , packageExtraSourceFiles = extraSourceFiles
      , packageExtraDocFiles = extraDocFiles
      , packageDataFiles = dataFiles
      , packageDataDir = packageConfigDataDir
      , packageSourceRepository = sourceRepository
      , packageCustomSetup = mCustomSetup
      , packageLibrary = mLibrary
      , packageInternalLibraries = internalLibraries
      , packageExecutables = executables
      , packageTests = tests
      , packageBenchmarks = benchmarks
      , packageVerbatim = fromMaybeList globalVerbatim
      }

  tell nameWarnings
  tell (formatMissingSourceDirs missingSourceDirs)
  return (determineCabalVersion inferredLicense pkg)
  where
    nameWarnings :: [String]
    packageName_ :: String
    (nameWarnings, packageName_) = case packageConfigName of
      Nothing -> let inferredName = takeBaseName dir in
        (["Package name not specified, inferred " ++ show inferredName], inferredName)
      Just n -> ([], n)

    mCustomSetup :: Maybe CustomSetup
    mCustomSetup = toCustomSetup <$> packageConfigCustomSetup

    flags = map toFlag $ toList packageConfigFlags

    toList :: Maybe (Map String a) -> [(String, a)]
    toList = Map.toList . fromMaybe mempty

    formatMissingSourceDirs = map f
      where
        f name = "Specified source-dir " ++ show name ++ " does not exist"

    sourceRepository :: Maybe SourceRepository
    sourceRepository = github <|> (`SourceRepository` Nothing) <$> packageConfigGit

    github :: Maybe SourceRepository
    github = parseGithub <$> packageConfigGithub
      where
        parseGithub :: Text -> SourceRepository
        parseGithub input = case map T.unpack $ T.splitOn "/" input of
          [owner, repo, subdir] ->
            SourceRepository (githubBaseUrl ++ owner ++ "/" ++ repo) (Just subdir)
          _ -> SourceRepository (githubBaseUrl ++ T.unpack input) Nothing

    homepage :: Maybe String
    homepage = case packageConfigHomepage of
      Just Nothing -> Nothing
      _ -> join packageConfigHomepage <|> fromGithub
      where
        fromGithub = (++ "#readme") . sourceRepositoryUrl <$> github

    bugReports :: Maybe String
    bugReports = case packageConfigBugReports of
      Just Nothing -> Nothing
      _ -> join packageConfigBugReports <|> fromGithub
      where
        fromGithub = (++ "/issues") . sourceRepositoryUrl <$> github

    maintainer :: Maybe (List String)
    maintainer = case (packageConfigAuthor, packageConfigMaintainer) of
      (Just _, Nothing) -> packageConfigAuthor
      (_, Just m) -> m
      _            -> Nothing

expandForeignSources
  :: MonadIO m
  => FilePath
  -> Traverse (Warnings m) ParseCSources CSources ParseCxxSources CxxSources ParseJsSources JsSources
expandForeignSources dir = Traverse {
    traverseCSources = expand "c-sources"
  , traverseCxxSources = expand "cxx-sources"
  , traverseJsSources = expand "js-sources"
  }
  where
    expand fieldName xs = do
      expandGlobs fieldName dir (fromMaybeList xs)

newtype Path = Path { unPath :: FilePath }
  deriving (Eq, Show, Ord)

instance IsString Path where
  fromString = Path

expandGlobs :: MonadIO m => String -> FilePath -> [String] -> Warnings m [Path]
expandGlobs name dir patterns = map Path <$> do
  (warnings, files) <- liftIO $ Util.expandGlobs name dir patterns
  tell warnings
  return files

toCustomSetup :: CustomSetupSection -> CustomSetup
toCustomSetup CustomSetupSection{..} = CustomSetup
  { customSetupDependencies = fromMaybe mempty customSetupSectionDependencies }

traverseSectionAndConditionals :: Monad m
  => (acc -> Section a -> m (acc, b))
  -> (acc -> Section a -> m (acc, b))
  -> acc
  -> Section a
  -> m (Section b)
traverseSectionAndConditionals fData fConditionals acc0 sect@Section{..} = do
  (acc1, x) <- fData acc0 sect
  xs <- traverseConditionals acc1 sectionConditionals
  return sect{sectionData = x, sectionConditionals = xs}
  where
    traverseConditionals = traverse . traverse . traverseSectionAndConditionals fConditionals fConditionals

getMentionedLibraryModules :: LibrarySection -> [String]
getMentionedLibraryModules (LibrarySection _ exposedModules generatedExposedModules otherModules generatedOtherModules _ _)
  = fromMaybeList (exposedModules <> generatedExposedModules <> otherModules <> generatedOtherModules)

listModules :: FilePath -> Section a -> IO [String]
listModules dir Section{..} = concat <$> mapM (getModules dir) sectionSourceDirs

inferModules ::
     FilePath
  -> String
  -> (a -> [String])
  -> (b -> [String])
  -> ([String] -> [String] -> a -> b)
  -> ([String] -> a -> b)
  -> Section a
  -> IO (Section b)
inferModules dir packageName_ getMentionedModules getInferredModules fromData fromConditionals = traverseSectionAndConditionals
  (fromConfigSection fromData [pathsModuleFromPackageName packageName_])
  (fromConfigSection (\ [] -> fromConditionals) [])
  []
  where
    fromConfigSection fromConfig pathsModule_ outerModules sect@Section{sectionData = conf} = do
      modules <- listModules dir sect
      let
        mentionedModules = concatMap getMentionedModules sect
        inferableModules = (modules \\ outerModules) \\ mentionedModules
        pathsModule = (pathsModule_ \\ outerModules) \\ mentionedModules
        r = fromConfig pathsModule inferableModules conf
      return (outerModules ++ getInferredModules r, r)

toLibrary :: FilePath -> String -> Section LibrarySection -> IO (Section Library)
toLibrary dir name =
    inferModules dir name getMentionedLibraryModules getLibraryModules fromLibrarySectionTopLevel fromLibrarySectionInConditional
  where
    getLibraryModules :: Library -> [String]
    getLibraryModules Library{..} = libraryExposedModules ++ libraryOtherModules

    fromLibrarySectionTopLevel pathsModule inferableModules LibrarySection{..} =
      Library librarySectionExposed exposedModules otherModules generatedModules reexportedModules signatures
      where
        (exposedModules, otherModules, generatedModules) =
          determineModules pathsModule inferableModules librarySectionExposedModules librarySectionGeneratedExposedModules librarySectionOtherModules librarySectionGeneratedOtherModules
        reexportedModules = fromMaybeList librarySectionReexportedModules
        signatures = fromMaybeList librarySectionSignatures

determineModules :: [String] -> [String] -> Maybe (List String) -> Maybe (List String) -> Maybe (List String) -> Maybe (List String) -> ([String], [String], [String])
determineModules pathsModule inferable mExposed mGeneratedExposed mOther mGeneratedOther =
  (exposed, others, generated)
  where
    generated = fromMaybeList (mGeneratedExposed <> mGeneratedOther)
    exposed = maybe inferable fromList mExposed ++ fromMaybeList mGeneratedExposed
    others = maybe ((inferable \\ exposed) ++ pathsModule) fromList mOther ++ fromMaybeList mGeneratedOther

fromLibrarySectionInConditional :: [String] -> LibrarySection -> Library
fromLibrarySectionInConditional inferableModules lib@(LibrarySection _ exposedModules _ otherModules _ _ _) =
  case (exposedModules, otherModules) of
    (Nothing, Nothing) -> addToOtherModules inferableModules (fromLibrarySectionPlain lib)
    _ -> fromLibrarySectionPlain lib
  where
    addToOtherModules xs r = r {libraryOtherModules = xs ++ libraryOtherModules r}

fromLibrarySectionPlain :: LibrarySection -> Library
fromLibrarySectionPlain LibrarySection{..} = Library {
    libraryExposed = librarySectionExposed
  , libraryExposedModules = fromMaybeList (librarySectionExposedModules <> librarySectionGeneratedExposedModules)
  , libraryOtherModules = fromMaybeList (librarySectionOtherModules <> librarySectionGeneratedOtherModules)
  , libraryGeneratedModules = fromMaybeList (librarySectionGeneratedOtherModules <> librarySectionGeneratedExposedModules)
  , libraryReexportedModules = fromMaybeList librarySectionReexportedModules
  , librarySignatures = fromMaybeList librarySectionSignatures
  }

getMentionedExecutableModules :: ExecutableSection -> [String]
getMentionedExecutableModules (ExecutableSection main otherModules generatedModules)=
  maybe id (:) (main >>= toModule . splitDirectories) $ fromMaybeList (otherModules <> generatedModules)

toExecutable :: FilePath -> String -> Section ExecutableSection -> IO (Section Executable)
toExecutable dir packageName_ =
    inferModules dir packageName_ getMentionedExecutableModules executableOtherModules fromExecutableSection (fromExecutableSection [])
  . expandMain
  where
    fromExecutableSection :: [String] -> [String] -> ExecutableSection -> Executable
    fromExecutableSection pathsModule inferableModules ExecutableSection{..} =
      (Executable executableSectionMain (otherModules ++ generatedModules) generatedModules)
      where
        otherModules = maybe (inferableModules ++ pathsModule) fromList executableSectionOtherModules
        generatedModules = maybe [] fromList executableSectionGeneratedOtherModules

expandMain :: Section ExecutableSection -> Section ExecutableSection
expandMain = flatten . expand
  where
    expand :: Section ExecutableSection -> Section ([GhcOption], ExecutableSection)
    expand = fmap go
      where
        go exec@ExecutableSection{..} =
          let
            (mainSrcFile, ghcOptions) = maybe (Nothing, []) (first Just . parseMain) executableSectionMain
          in
            (ghcOptions, exec{executableSectionMain = mainSrcFile})

    flatten :: Section ([GhcOption], ExecutableSection) -> Section ExecutableSection
    flatten sect@Section{sectionData = (ghcOptions, exec), ..} = sect{
        sectionData = exec
      , sectionGhcOptions = sectionGhcOptions ++ ghcOptions
      , sectionConditionals = map (fmap flatten) sectionConditionals
      }

toSection :: Monad m => String -> [String] -> WithCommonOptions CSources CxxSources JsSources a -> Warnings m (Section a)
toSection packageName_ executableNames = go
  where
    go (Product CommonOptions{..} a) = do
      (systemBuildTools, buildTools) <- maybe (return mempty) toBuildTools commonOptionsBuildTools

      conditionals <- mapM toConditional (fromMaybeList commonOptionsWhen)
      return Section {
        sectionData = a
      , sectionSourceDirs = fromMaybeList commonOptionsSourceDirs
      , sectionDefaultExtensions = fromMaybeList commonOptionsDefaultExtensions
      , sectionOtherExtensions = fromMaybeList commonOptionsOtherExtensions
      , sectionGhcOptions = fromMaybeList commonOptionsGhcOptions
      , sectionGhcProfOptions = fromMaybeList commonOptionsGhcProfOptions
      , sectionGhcjsOptions = fromMaybeList commonOptionsGhcjsOptions
      , sectionCppOptions = fromMaybeList commonOptionsCppOptions
      , sectionCcOptions = fromMaybeList commonOptionsCcOptions
      , sectionCSources = commonOptionsCSources
      , sectionCxxOptions = fromMaybeList commonOptionsCxxOptions
      , sectionCxxSources = commonOptionsCxxSources
      , sectionJsSources = commonOptionsJsSources
      , sectionExtraLibDirs = fromMaybeList commonOptionsExtraLibDirs
      , sectionExtraLibraries = fromMaybeList commonOptionsExtraLibraries
      , sectionExtraFrameworksDirs = fromMaybeList commonOptionsExtraFrameworksDirs
      , sectionFrameworks = fromMaybeList commonOptionsFrameworks
      , sectionIncludeDirs = fromMaybeList commonOptionsIncludeDirs
      , sectionInstallIncludes = fromMaybeList commonOptionsInstallIncludes
      , sectionLdOptions = fromMaybeList commonOptionsLdOptions
      , sectionBuildable = commonOptionsBuildable
      , sectionDependencies = fromMaybe mempty commonOptionsDependencies
      , sectionPkgConfigDependencies = fromMaybeList commonOptionsPkgConfigDependencies
      , sectionConditionals = conditionals
      , sectionBuildTools = buildTools
      , sectionSystemBuildTools = systemBuildTools <> fromMaybe mempty commonOptionsSystemBuildTools
      , sectionVerbatim = fromMaybeList commonOptionsVerbatim
      }
    toBuildTools :: Monad m => BuildTools -> Warnings m (SystemBuildTools, Map BuildTool DependencyVersion)
    toBuildTools = fmap (mkSystemBuildTools &&& mkBuildTools) . mapM (toBuildTool packageName_ executableNames). unBuildTools
      where
        mkSystemBuildTools :: [Either (String, VersionConstraint) b] -> SystemBuildTools
        mkSystemBuildTools = SystemBuildTools . Map.fromList . lefts

        mkBuildTools = Map.fromList . rights

    toConditional :: Monad m => ConditionalSection CSources CxxSources JsSources a -> Warnings m (Conditional (Section a))
    toConditional x = case x of
      ThenElseConditional (Product (ThenElse then_ else_) c) -> conditional c <$> (go then_) <*> (Just <$> go else_)
      FlatConditional (Product sect c) -> conditional c <$> (go sect) <*> pure Nothing
      where
        conditional (Condition (Cond c)) = Conditional c

type SystemBuildTool = (String, VersionConstraint)

toBuildTool :: Monad m => String -> [String] -> (ParseBuildTool, DependencyVersion)
  -> Warnings m (Either SystemBuildTool (BuildTool, DependencyVersion))
toBuildTool packageName_ executableNames = \ case
  (QualifiedBuildTool pkg executable, v)
    | pkg == packageName_ && executable `elem` executableNames -> localBuildTool executable v
    | otherwise -> buildTool pkg executable v
  (UnqualifiedBuildTool executable, v)
    | executable `elem` executableNames -> localBuildTool executable v
    | Just pkg <- lookup executable legacyTools -> legacyBuildTool pkg executable v
    | executable `elem` legacySystemTools, DependencyVersion Nothing c <- v -> legacySystemBuildTool executable c
    | otherwise -> buildTool executable executable v
  where
    buildTool pkg executable v = return . Right $ (BuildTool pkg executable, v)

    systemBuildTool = return . Left

    localBuildTool executable v = return . Right $ (LocalBuildTool executable, v)
    legacyBuildTool pkg executable v = warnLegacyTool pkg executable >> buildTool pkg executable v
    legacySystemBuildTool executable c = warnLegacySystemTool executable >> systemBuildTool (executable, c)

    legacyTools = [
        ("gtk2hsTypeGen", "gtk2hs-buildtools")
      , ("gtk2hsHookGenerator", "gtk2hs-buildtools")
      , ("gtk2hsC2hs", "gtk2hs-buildtools")
      , ("cabal", "cabal-install")
      , ("grgen", "cgen")
      , ("cgen-hs", "cgen")
      ]
    legacySystemTools = [
        "ghc"
      , "git"
      , "llvm-config"
      , "gfortran"
      , "gcc"
      , "couchdb"
      , "mcc"
      , "nix-store"
      , "nix-instantiate"
      , "nix-hash"
      , "nix-env"
      , "nix-build"
      ]
    warnLegacyTool pkg name = tell ["Usage of the unqualified build-tool name " ++ show name ++ " is deprecated! Please use the qualified name \"" ++ pkg ++ ":" ++ name ++ "\" instead!"]
    warnLegacySystemTool name = tell ["Listing " ++ show name ++ " under build-tools is deperecated! Please list system executables under system-build-tools instead!"]

pathsModuleFromPackageName :: String -> String
pathsModuleFromPackageName name = "Paths_" ++ map f name
  where
    f '-' = '_'
    f x = x

getModules :: FilePath -> FilePath -> IO [String]
getModules dir src_ = sort <$> do
  exists <- doesDirectoryExist (dir </> src_)
  if exists
    then do
      src <- canonicalizePath (dir </> src_)
      removeSetup src . toModules <$> getModuleFilesRecursive src
    else return []
  where
    toModules :: [[FilePath]] -> [String]
    toModules = catMaybes . map toModule

    removeSetup :: FilePath -> [String] -> [String]
    removeSetup src
      | src == dir = filter (/= "Setup")
      | otherwise = id
