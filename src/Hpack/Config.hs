{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}
module Hpack.Config (
  packageConfig
, readPackageConfig
, renamePackage
, packageDependencies
, package
, section
, Package(..)
, Dependencies(..)
, DependencyVersion(..)
, SourceDependency(..)
, GitRef
, GitUrl
, GhcOption
, CustomSetup(..)
, Section(..)
, Library(..)
, Executable(..)
, Conditional(..)
, Flag(..)
, SourceRepository(..)
#ifdef TEST
, renameDependencies
, Empty(..)
, getModules
, determineModules
, BuildType(..)
#endif
) where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson.Types
import           Data.Data
import           Data.Bifunctor
import           Data.Bifoldable
import           Data.Bitraversable
import           Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import qualified Data.HashMap.Lazy as HashMap
import           Data.List (nub, (\\), sortBy)
import           Data.Maybe
import           Data.Monoid hiding (Product)
import           Data.Ord
import           Data.String
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.Generics (Generic, Rep)
import           System.Directory
import           System.FilePath
import           Data.Functor.Identity
import           Control.Monad.Trans.Writer
import           Control.Monad.IO.Class

import           Hpack.GenericsUtil
import           Hpack.UnknownFields
import           Hpack.Util
import           Hpack.Yaml
import           Hpack.Dependency

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
  , packageDataFiles = []
  , packageSourceRepository = Nothing
  , packageCustomSetup = Nothing
  , packageLibrary = Nothing
  , packageInternalLibraries = mempty
  , packageExecutables = mempty
  , packageTests = mempty
  , packageBenchmarks = mempty
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

packageDependencies :: Package -> [(String, DependencyVersion)]
packageDependencies Package{..} = nub . sortBy (comparing (lexicographically . fst)) $
     (concatMap deps packageExecutables)
  ++ (concatMap deps packageTests)
  ++ (concatMap deps packageBenchmarks)
  ++ maybe [] deps packageLibrary
  where
    deps xs = [(name, version) | (name, version) <- (Map.toList . unDependencies . sectionDependencies) xs]

section :: a -> Section a
section a = Section a [] mempty [] [] [] [] [] [] [] [] [] [] [] [] [] [] [] [] Nothing [] mempty

packageConfig :: FilePath
packageConfig = "package.yaml"

#if MIN_VERSION_aeson(1,0,0)
genericParseJSON_ :: forall a d m. (GFromJSON Zero (Rep a), HasTypeName a d m) => Value -> Parser a
#else
genericParseJSON_ :: forall a d m. (GFromJSON (Rep a), HasTypeName a d m) => Value -> Parser a
#endif
genericParseJSON_ = genericParseJSON defaultOptions {fieldLabelModifier = hyphenize name}
  where
    name :: String
    name = typeName (Proxy :: Proxy a)

data CustomSetupSection = CustomSetupSection {
  customSetupSectionDependencies :: Maybe Dependencies
} deriving (Eq, Show, Generic)

instance HasFieldNames CustomSetupSection

instance FromJSON CustomSetupSection where
  parseJSON = genericParseJSON_

data LibrarySection = LibrarySection {
  librarySectionExposed :: Maybe Bool
, librarySectionExposedModules :: Maybe (List String)
, librarySectionOtherModules :: Maybe (List String)
, librarySectionReexportedModules :: Maybe (List String)
} deriving (Eq, Show, Generic)

emptyLibrarySection :: LibrarySection
emptyLibrarySection = LibrarySection Nothing Nothing Nothing Nothing

instance HasFieldNames LibrarySection

instance FromJSON LibrarySection where
  parseJSON = genericParseJSON_

data ExecutableSection = ExecutableSection {
  executableSectionMain :: Maybe FilePath
, executableSectionOtherModules :: Maybe (List String)
} deriving (Eq, Show, Generic)

emptyExecutableSection :: ExecutableSection
emptyExecutableSection = ExecutableSection Nothing Nothing

instance HasFieldNames ExecutableSection

instance FromJSON ExecutableSection where
  parseJSON = genericParseJSON_

data CommonOptions a capture cSources jsSources = CommonOptions {
  commonOptionsSourceDirs :: Maybe (List FilePath)
, commonOptionsDependencies :: Maybe Dependencies
, commonOptionsDefaultExtensions :: Maybe (List String)
, commonOptionsOtherExtensions :: Maybe (List String)
, commonOptionsGhcOptions :: Maybe (List GhcOption)
, commonOptionsGhcProfOptions :: Maybe (List GhcProfOption)
, commonOptionsGhcjsOptions :: Maybe (List GhcjsOption)
, commonOptionsCppOptions :: Maybe (List CppOption)
, commonOptionsCcOptions :: Maybe (List CcOption)
, commonOptionsCSources :: cSources
, commonOptionsJsSources :: jsSources
, commonOptionsExtraLibDirs :: Maybe (List FilePath)
, commonOptionsExtraLibraries :: Maybe (List FilePath)
, commonOptionsExtraFrameworksDirs :: Maybe (List FilePath)
, commonOptionsFrameworks :: Maybe (List String)
, commonOptionsIncludeDirs :: Maybe (List FilePath)
, commonOptionsInstallIncludes :: Maybe (List FilePath)
, commonOptionsLdOptions :: Maybe (List LdOption)
, commonOptionsBuildable :: Maybe Bool
, commonOptionsWhen :: Maybe (List (ConditionalSection a capture cSources jsSources))
, commonOptionsBuildTools :: Maybe Dependencies
} deriving Generic

type ParseCommonOptions a = CommonOptions a CaptureUnknownFields ParseCSources ParseJsSources

instance HasFieldNames (ParseCommonOptions a)

instance (FromJSON a, HasFieldNames a) => FromJSON (ParseCommonOptions a) where
  parseJSON = genericParseJSON_

type ParseCSources = Maybe (List FilePath)
type ParseJsSources = Maybe (List FilePath)

type CSources = [FilePath]
type JsSources = [FilePath]

type WithCommonOptions a capture cSources jsSources = Product (CommonOptions a capture cSources jsSources) a

data Traverse m capture capture_ cSources cSources_ jsSources jsSources_ = Traverse {
  traverseCapture :: forall a. capture a -> m (capture_ a)
, traverseCSources :: cSources -> m cSources_
, traverseJsSources :: jsSources -> m jsSources_
}

type Traversal t = forall m capture capture_ cSources cSources_ jsSources jsSources_. (Monad m, Traversable capture_)
  => Traverse m capture capture_ cSources cSources_ jsSources jsSources_
  -> t capture cSources jsSources
  -> m (t capture_ cSources_ jsSources_)

traverseCommonOptions :: Traversal (CommonOptions a)
traverseCommonOptions t@Traverse{..} c@CommonOptions{..} = do
  cSources <- traverseCSources commonOptionsCSources
  jsSources <- traverseJsSources commonOptionsJsSources
  xs <- traverse (traverse (traverseConditionalSection t)) commonOptionsWhen
  return c {
      commonOptionsCSources = cSources
    , commonOptionsJsSources = jsSources
    , commonOptionsWhen = xs
    }

traverseConditionalSection :: Traversal (ConditionalSection a)
traverseConditionalSection t@Traverse{..} = \ case
  ThenElseConditional c -> ThenElseConditional <$> (traverseCapture c >>= traverse (traverseThenElse t))
  FlatConditional c -> FlatConditional <$> (traverseCapture c >>= traverse (bitraverse (traverseWithCommonOptions t) return))

traverseThenElse :: Traversal (ThenElse a)
traverseThenElse t@Traverse{..} c@ThenElse{..} = do
  then_ <- traverseCapture thenElseThen >>= traverse (traverseWithCommonOptions t)
  else_ <- traverseCapture thenElseElse >>= traverse (traverseWithCommonOptions t)
  return c{thenElseThen = then_, thenElseElse = else_}

traverseWithCommonOptions :: Traversal (WithCommonOptions a)
traverseWithCommonOptions t = bitraverse (traverseCommonOptions t) return

data Product a b = Product a b
  deriving (Eq, Show)

instance Bifunctor Product where
  bimap fa fb (Product a b) = Product (fa a) (fb b)

instance Bifoldable Product where
  bifoldMap = bifoldMapDefault

instance Bitraversable Product where
  bitraverse fa fb (Product a b) = Product <$> fa a <*> fb b

instance (FromJSON a, FromJSON b) => FromJSON (Product a b) where
  parseJSON value = Product <$> parseJSON value <*> parseJSON value

instance (HasFieldNames a, HasFieldNames b) => HasFieldNames (Product a b) where
  fieldNames Proxy =
       fieldNames (Proxy :: Proxy a)
    ++ fieldNames (Proxy :: Proxy b)
  ignoreUnderscoredUnknownFields Proxy =
       ignoreUnderscoredUnknownFields (Proxy :: Proxy a)
    || ignoreUnderscoredUnknownFields (Proxy :: Proxy b)

data ConditionalSection a capture cSources jsSources =
    ThenElseConditional (capture (ThenElse a capture cSources jsSources))
  | FlatConditional (capture (Product (WithCommonOptions a capture cSources jsSources) Condition))

type ParseConditionalSection a = ConditionalSection a CaptureUnknownFields ParseCSources ParseJsSources

instance (FromJSON a, HasFieldNames a) => FromJSON (ParseConditionalSection a) where
  parseJSON v
    | hasKey "then" v || hasKey "else" v = ThenElseConditional <$> parseJSON v
    | otherwise = FlatConditional <$> parseJSON v

hasKey :: Text -> Value -> Bool
hasKey key (Object o) = HashMap.member key o
hasKey _ _ = False

newtype Condition = Condition {
  conditionCondition :: String
} deriving (Eq, Show, Generic)

instance FromJSON Condition where
  parseJSON = genericParseJSON_

instance HasFieldNames Condition

data ThenElse a capture cSources jsSources = ThenElse {
  _thenElseCondition :: String
, thenElseThen :: capture (WithCommonOptions a capture cSources jsSources)
, thenElseElse :: capture (WithCommonOptions a capture cSources jsSources)
} deriving Generic

type ParseThenElse a = ThenElse a CaptureUnknownFields ParseCSources ParseJsSources

instance HasFieldNames (ParseThenElse a)

instance (FromJSON a, HasFieldNames a) => FromJSON (ParseThenElse a) where
  parseJSON = genericParseJSON_

data Empty = Empty
  deriving (Eq, Show)

instance FromJSON Empty where
  parseJSON _ = return Empty

instance HasFieldNames Empty where
  fieldNames _ = []

-- From Cabal the library, copied here to avoid a dependency on Cabal.
data BuildType
  = Simple
  | Configure
  | Make
  | Custom
  deriving (Eq, Show, Generic)

instance FromJSON BuildType where
  parseJSON = withText "String" $ \case
    "Simple"    -> return Simple
    "Configure" -> return Configure
    "Make"      -> return Make
    "Custom"    -> return Custom
    _           -> fail "build-type must be one of: Simple, Configure, Make, Custom"

type SectionConfig a capture cSources jsSources = capture (WithCommonOptions a capture cSources jsSources)

data PackageConfig capture cSources jsSources = PackageConfig {
  packageConfigName :: Maybe String
, packageConfigVersion :: Maybe String
, packageConfigSynopsis :: Maybe String
, packageConfigDescription :: Maybe String
, packageConfigHomepage :: Maybe (Maybe String)
, packageConfigBugReports :: Maybe (Maybe String)
, packageConfigCategory :: Maybe String
, packageConfigStability :: Maybe String
, packageConfigAuthor :: Maybe (List String)
, packageConfigMaintainer :: Maybe (List String)
, packageConfigCopyright :: Maybe (List String)
, packageConfigBuildType :: Maybe BuildType
, packageConfigLicense :: Maybe String
, packageConfigLicenseFile :: Maybe (List String)
, packageConfigTestedWith :: Maybe String
, packageConfigFlags :: Maybe (Map String (capture FlagSection))
, packageConfigExtraSourceFiles :: Maybe (List FilePath)
, packageConfigDataFiles :: Maybe (List FilePath)
, packageConfigGithub :: Maybe Text
, packageConfigGit :: Maybe String
, packageConfigCustomSetup :: Maybe (capture CustomSetupSection)
, packageConfigLibrary :: Maybe (SectionConfig LibrarySection capture cSources jsSources)
, packageConfigInternalLibraries :: Maybe (Map String (SectionConfig LibrarySection capture cSources jsSources))
, packageConfigExecutable :: Maybe (SectionConfig ExecutableSection capture cSources jsSources)
, packageConfigExecutables :: Maybe (Map String (SectionConfig ExecutableSection capture cSources jsSources))
, packageConfigTests :: Maybe (Map String (SectionConfig ExecutableSection capture cSources jsSources))
, packageConfigBenchmarks :: Maybe (Map String (SectionConfig ExecutableSection capture cSources jsSources))
} deriving Generic

traversePackageConfig :: Traversal PackageConfig
traversePackageConfig t@Traverse{..} p@PackageConfig{..} = do
  flags <- traverse (traverse traverseCapture) packageConfigFlags
  customSetup <- traverse traverseCapture packageConfigCustomSetup
  library <- traverse (traverseSectionConfig t) packageConfigLibrary
  internalLibraries <- traverseNamedConfigs t packageConfigInternalLibraries
  executable <- traverse (traverseSectionConfig t) packageConfigExecutable
  executables <- traverseNamedConfigs t packageConfigExecutables
  tests <- traverseNamedConfigs t packageConfigTests
  benchmarks <- traverseNamedConfigs t packageConfigBenchmarks
  return p {
      packageConfigFlags = flags
    , packageConfigCustomSetup = customSetup
    , packageConfigLibrary = library
    , packageConfigInternalLibraries = internalLibraries
    , packageConfigExecutable = executable
    , packageConfigExecutables = executables
    , packageConfigTests = tests
    , packageConfigBenchmarks = benchmarks
    }
  where
    traverseNamedConfigs = traverse . traverse . traverseSectionConfig

traverseSectionConfig :: Traversal (SectionConfig p)
traverseSectionConfig t = traverseCapture t >=> traverse (traverseWithCommonOptions t)

type ParsePackageConfig = PackageConfig CaptureUnknownFields ParseCSources ParseJsSources

instance HasFieldNames ParsePackageConfig where
  ignoreUnderscoredUnknownFields _ = True

instance FromJSON ParsePackageConfig where
  parseJSON value = handleNullValues <$> genericParseJSON_ value
    where
      handleNullValues :: ParsePackageConfig -> ParsePackageConfig
      handleNullValues =
          ifNull "homepage" (\p -> p {packageConfigHomepage = Just Nothing})
        . ifNull "bug-reports" (\p -> p {packageConfigBugReports = Just Nothing})

      ifNull :: String -> (a -> a) -> a -> a
      ifNull name f
        | isNull name value = f
        | otherwise = id

isNull :: String -> Value -> Bool
isNull name value = case parseMaybe p value of
  Just Null -> True
  _ -> False
  where
    p = parseJSON >=> (.: fromString name)

readPackageConfig :: FilePath -> IO (Either String ([String], Package))
readPackageConfig file = do
  r <- decodeYaml file
  case r of
    Left err -> return (Left err)
    Right config -> do
      dir <- takeDirectory <$> canonicalizePath file
      Right <$> toPackage dir config

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
, packageExtraSourceFiles :: [FilePath]
, packageDataFiles :: [FilePath]
, packageSourceRepository :: Maybe SourceRepository
, packageCustomSetup :: Maybe CustomSetup
, packageLibrary :: Maybe (Section Library)
, packageInternalLibraries :: Map String (Section Library)
, packageExecutables :: Map String (Section Executable)
, packageTests :: Map String (Section Executable)
, packageBenchmarks :: Map String (Section Executable)
} deriving (Eq, Show)

data CustomSetup = CustomSetup {
  customSetupDependencies :: Dependencies
} deriving (Eq, Show)

data Library = Library {
  libraryExposed :: Maybe Bool
, libraryExposedModules :: [String]
, libraryOtherModules :: [String]
, libraryReexportedModules :: [String]
} deriving (Eq, Show)

data Executable = Executable {
  executableMain :: Maybe FilePath
, executableOtherModules :: [String]
} deriving (Eq, Show)

data Section a = Section {
  sectionData :: a
, sectionSourceDirs :: [FilePath]
, sectionDependencies :: Dependencies
, sectionDefaultExtensions :: [String]
, sectionOtherExtensions :: [String]
, sectionGhcOptions :: [GhcOption]
, sectionGhcProfOptions :: [GhcProfOption]
, sectionGhcjsOptions :: [GhcjsOption]
, sectionCppOptions :: [CppOption]
, sectionCcOptions :: [CcOption]
, sectionCSources :: [FilePath]
, sectionJsSources :: [FilePath]
, sectionExtraLibDirs :: [FilePath]
, sectionExtraLibraries :: [FilePath]
, sectionExtraFrameworksDirs :: [FilePath]
, sectionFrameworks :: [FilePath]
, sectionIncludeDirs :: [FilePath]
, sectionInstallIncludes :: [FilePath]
, sectionLdOptions :: [LdOption]
, sectionBuildable :: Maybe Bool
, sectionConditionals :: [Conditional (Section a)]
, sectionBuildTools :: Dependencies
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
} deriving (Eq, Show, Generic)

instance HasFieldNames FlagSection

instance FromJSON FlagSection where
  parseJSON = genericParseJSON_

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

type Config capture cSources jsSources =
  Product (CommonOptions Empty capture cSources jsSources) (PackageConfig capture cSources jsSources)

traverseConfig :: Traversal Config
traverseConfig t = bitraverse (traverseCommonOptions t) (traversePackageConfig t)

type ParseConfig = CaptureUnknownFields (Config CaptureUnknownFields ParseCSources ParseJsSources)

toPackage :: FilePath -> ParseConfig -> IO ([String], Package)
toPackage dir = runWriterT . (extractUnknownFieldWarnings >=> expandForeignSources dir) >=> toPackage_ dir

toPackage_ :: FilePath -> (Config Identity CSources JsSources, [String]) -> IO ([String], Package)
toPackage_ dir (Product (toSection . (`Product` Empty) -> globalOptions) PackageConfig{..}, packageWarnings) = do
  mLibrary <- mapM (toLibrary dir packageName_ globalOptions) mLibrarySection

  let
    executableSections :: Map String (Section ExecutableSection)
    (executableWarning, executableSections) = (warning, sections)
      where
        sections :: Map String (Section ExecutableSection)
        sections = case mExecutable of
          Just executable -> Map.fromList [(packageName_, executable)]
          Nothing -> executables

        warning = case mExecutable of
          Just _ | not (null executables) -> ["Ignoring field \"executables\" in favor of \"executable\""]
          _ -> []

        executables = toSections packageConfigExecutables

        mExecutable :: Maybe (Section ExecutableSection)
        mExecutable = toSectionI <$> packageConfigExecutable

  internalLibraries <- toInternalLibraries dir packageName_ globalOptions internalLibrariesSections
  executables <- toExecutables dir packageName_ globalOptions executableSections
  tests <- toExecutables dir packageName_ globalOptions testSections
  benchmarks <- toExecutables dir packageName_ globalOptions benchmarkSections

  licenseFileExists <- doesFileExist (dir </> "LICENSE")

  missingSourceDirs <- nub . sort <$> filterM (fmap not <$> doesDirectoryExist . (dir </>)) (
       maybe [] sectionSourceDirs mLibrary
    ++ concatMap sectionSourceDirs internalLibraries
    ++ concatMap sectionSourceDirs executables
    ++ concatMap sectionSourceDirs tests
    ++ concatMap sectionSourceDirs benchmarks
    )

  (extraSourceFilesWarnings, extraSourceFiles) <-
    expandGlobs "extra-source-files" dir (fromMaybeList packageConfigExtraSourceFiles)

  (dataFilesWarnings, dataFiles) <-
    expandGlobs "data-files" dir (fromMaybeList packageConfigDataFiles)

  let defaultBuildType :: BuildType
      defaultBuildType = maybe Simple (const Custom) mCustomSetup

      configLicenseFiles :: Maybe (List String)
      configLicenseFiles = packageConfigLicenseFile <|> do
        guard licenseFileExists
        Just (List ["LICENSE"])

      pkg = Package {
        packageName = packageName_
      , packageVersion = fromMaybe "0.0.0" packageConfigVersion
      , packageSynopsis = packageConfigSynopsis
      , packageDescription = packageConfigDescription
      , packageHomepage = homepage
      , packageBugReports = bugReports
      , packageCategory = packageConfigCategory
      , packageStability = packageConfigStability
      , packageAuthor = fromMaybeList packageConfigAuthor
      , packageMaintainer = fromMaybeList packageConfigMaintainer
      , packageCopyright = fromMaybeList packageConfigCopyright
      , packageBuildType = fromMaybe defaultBuildType packageConfigBuildType
      , packageLicense = packageConfigLicense
      , packageLicenseFile = fromMaybeList configLicenseFiles
      , packageTestedWith = packageConfigTestedWith
      , packageFlags = flags
      , packageExtraSourceFiles = extraSourceFiles
      , packageDataFiles = dataFiles
      , packageSourceRepository = sourceRepository
      , packageCustomSetup = mCustomSetup
      , packageLibrary = mLibrary
      , packageInternalLibraries = internalLibraries
      , packageExecutables = executables
      , packageTests = tests
      , packageBenchmarks = benchmarks
      }

      warnings =
           packageWarnings
        ++ nameWarnings
        ++ formatMissingSourceDirs missingSourceDirs
        ++ executableWarning
        ++ extraSourceFilesWarnings
        ++ dataFilesWarnings

  return (warnings, pkg)
  where
    nameWarnings :: [String]
    packageName_ :: String
    (nameWarnings, packageName_) = case packageConfigName of
      Nothing -> let inferredName = takeBaseName dir in
        (["Package name not specified, inferred " ++ show inferredName], inferredName)
      Just n -> ([], n)

    mCustomSetup :: Maybe CustomSetup
    mCustomSetup = toCustomSetup . runIdentity <$> packageConfigCustomSetup

    mLibrarySection :: Maybe (Section LibrarySection)
    mLibrarySection = toSectionI <$> packageConfigLibrary

    toSections :: Maybe (Map String (Identity (WithCommonOptions a Identity CSources JsSources))) -> Map String (Section a)
    toSections = fmap toSectionI . fromMaybe mempty

    internalLibrariesSections :: Map String (Section LibrarySection)
    internalLibrariesSections = toSections packageConfigInternalLibraries

    testSections :: Map String (Section ExecutableSection)
    testSections = toSections packageConfigTests

    benchmarkSections :: Map String (Section ExecutableSection)
    benchmarkSections = toSections packageConfigBenchmarks

    flags = map (toFlag . fmap runIdentity) $ toList packageConfigFlags

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
          [user, repo, subdir] ->
            SourceRepository (githubBaseUrl ++ user ++ "/" ++ repo) (Just subdir)
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

type Warnings m = WriterT [String] m

extractUnknownFieldWarnings :: forall m. Monad m => ParseConfig -> Warnings m (Config Identity ParseCSources ParseJsSources)
extractUnknownFieldWarnings = warnGlobal >=> bitraverse return warnSections
  where
    t :: Monad capture => Traverse capture capture Identity cSources cSources jsSources jsSources
    t = Traverse (fmap Identity) return return

    warnGlobal c = warnUnknownFields In "package description" (c >>= bitraverse (traverseCommonOptions t) return)

    warnSections :: ParsePackageConfig -> Warnings m (PackageConfig Identity ParseCSources ParseJsSources)
    warnSections p@PackageConfig{..} = do
      flags <- traverse (warnNamed For "flag" . fmap (traverseCapture t)) packageConfigFlags
      customSetup <- warnUnknownFields In "custom-setup section" (traverse (traverseCapture t) packageConfigCustomSetup)
      library <- warnUnknownFields In "library section" (traverse (traverseSectionConfig t) packageConfigLibrary)
      internalLibraries <- warnNamedSection "internal-libraries" packageConfigInternalLibraries
      executable <- warnUnknownFields In "executable section" (traverse (traverseSectionConfig t) packageConfigExecutable)
      executables <- warnNamedSection "executable" packageConfigExecutables
      tests <- warnNamedSection "test" packageConfigTests
      benchmarks <- warnNamedSection "benchmark" packageConfigBenchmarks
      return p {
          packageConfigFlags = flags
        , packageConfigCustomSetup = customSetup
        , packageConfigLibrary = library
        , packageConfigInternalLibraries = internalLibraries
        , packageConfigExecutable = executable
        , packageConfigExecutables = executables
        , packageConfigTests = tests
        , packageConfigBenchmarks = benchmarks
        }

    warnNamedSection
      :: String
      -> Maybe (Map String (SectionConfig a CaptureUnknownFields cSources jsSources))
      -> Warnings m (Maybe (Map String (SectionConfig a Identity cSources jsSources)))
    warnNamedSection sectionType = traverse (warnNamed In (sectionType ++ " section") . fmap (traverseSectionConfig t))

    warnNamed :: Preposition -> String -> Map String (CaptureUnknownFields a) -> Warnings m (Map String a)
    warnNamed preposition sect = fmap Map.fromList . mapM f . Map.toList
      where
        f (name, fields) = (,) name <$> (warnUnknownFields preposition (sect ++ " " ++ show name) fields)

    warnUnknownFields :: Preposition -> String -> CaptureUnknownFields a -> Warnings m a
    warnUnknownFields preposition name = fmap snd . bitraverse tell return . formatUnknownFields preposition name

expandForeignSources
  :: FilePath
  -> Config Identity ParseCSources ParseJsSources
  -> Warnings IO (Config Identity CSources JsSources)
expandForeignSources dir = traverseConfig t
  where
    t = Traverse {
      traverseCapture = return
    , traverseCSources = expand "c-sources"
    , traverseJsSources = expand "js-sources"
    }

    expand fieldName xs = do
      (warnings, files) <- liftIO $ expandGlobs fieldName dir (fromMaybeList xs)
      tell warnings
      return files

toCustomSetup :: CustomSetupSection -> CustomSetup
toCustomSetup CustomSetupSection{..} = CustomSetup
  { customSetupDependencies = fromMaybe mempty customSetupSectionDependencies }

traverseSection :: Applicative m => (Section a -> m b) -> Section a -> m (Section b)
traverseSection f = biTraverseSection f f

biTraverseSection :: Applicative m => (Section a -> m b) -> (Section a -> m b) -> Section a -> m (Section b)
biTraverseSection fData fConditionals sect =
  update <$> fData sect <*> traverse (traverse $ traverseSection fConditionals) (sectionConditionals sect)
  where
    update x xs = sect{sectionData = x, sectionConditionals = xs}

toLibrary :: FilePath -> String -> Section global -> Section LibrarySection -> IO (Section Library)
toLibrary dir name globalOptions library =
  biTraverseSection fromLibrarySection fromLibrarySectionInConditional sect
  where
    sect :: Section LibrarySection
    sect = mergeSections emptyLibrarySection globalOptions library

    mentionedModules = flip concatMap sect $ \ LibrarySection{..} ->
      fromMaybeList librarySectionExposedModules ++ fromMaybeList librarySectionOtherModules

    fromLibrarySection :: Section LibrarySection -> IO Library
    fromLibrarySection Section{sectionData = LibrarySection{..}, sectionSourceDirs = sourceDirs} = do
      modules <- concat <$> mapM (getModules dir) sourceDirs
      let
        inferableModules = modules \\ mentionedModules
        (exposedModules, otherModules) =
          determineModules name inferableModules librarySectionExposedModules librarySectionOtherModules
        reexportedModules = fromMaybeList librarySectionReexportedModules
      return (Library librarySectionExposed exposedModules otherModules reexportedModules)

    fromLibrarySectionInConditional :: Section LibrarySection -> IO Library
    fromLibrarySectionInConditional = return . fromLibrarySectionPlain . sectionData

    fromLibrarySectionPlain :: LibrarySection -> Library
    fromLibrarySectionPlain LibrarySection{..} = Library {
        libraryExposed = librarySectionExposed
      , libraryExposedModules = fromMaybeList librarySectionExposedModules
      , libraryOtherModules = fromMaybeList librarySectionOtherModules
      , libraryReexportedModules = fromMaybeList librarySectionReexportedModules
      }

toInternalLibraries :: FilePath -> String -> Section global -> Map String (Section LibrarySection) -> IO (Map String (Section Library))
toInternalLibraries dir packageName_ globalOptions = traverse (toLibrary dir packageName_ globalOptions)

toExecutables :: FilePath -> String -> Section global -> Map String (Section ExecutableSection) -> IO (Map String (Section Executable))
toExecutables dir packageName_ globalOptions = traverse (toExecutable dir packageName_ globalOptions)

toExecutable :: FilePath -> String -> Section global -> Section ExecutableSection -> IO (Section Executable)
toExecutable dir packageName_ globalOptions =
      toExecutable_ True . expandMain . mergeSections emptyExecutableSection globalOptions
  where
    toExecutable_ :: Bool -> Section ExecutableSection -> IO (Section Executable)
    toExecutable_ inferPathsModule sect@Section{..} = do
      executable <- fromExecutableSection sectionData
      conditionals <- mapM (traverse $ toExecutable_ False) sectionConditionals
      return sect {
          sectionData = executable
        , sectionConditionals = conditionals
        }
      where
        fromExecutableSection :: ExecutableSection -> IO Executable
        fromExecutableSection (ExecutableSection main_ otherModules)= do
          modules <- maybe inferModules (return . fromList) otherModules
          return (Executable main_ modules)
          where
            inferModules :: IO [String]
            inferModules
              | null sectionSourceDirs = return []
              | otherwise = filterMain . (++ pathsModule) . concat <$> mapM (getModules dir) sectionSourceDirs

            pathsModule = case inferPathsModule of
              True -> [pathsModuleFromPackageName packageName_]
              False -> []

            filterMain :: [String] -> [String]
            filterMain = maybe id (maybe id (filter . (/=)) . toModule . splitDirectories) main_

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

mergeSections :: a -> Section global -> Section a -> Section a
mergeSections a globalOptions options
  = Section {
    sectionData = sectionData options
  , sectionSourceDirs = sectionSourceDirs globalOptions ++ sectionSourceDirs options
  , sectionDefaultExtensions = sectionDefaultExtensions globalOptions ++ sectionDefaultExtensions options
  , sectionOtherExtensions = sectionOtherExtensions globalOptions ++ sectionOtherExtensions options
  , sectionGhcOptions = sectionGhcOptions globalOptions ++ sectionGhcOptions options
  , sectionGhcProfOptions = sectionGhcProfOptions globalOptions ++ sectionGhcProfOptions options
  , sectionGhcjsOptions = sectionGhcjsOptions globalOptions ++ sectionGhcjsOptions options
  , sectionCppOptions = sectionCppOptions globalOptions ++ sectionCppOptions options
  , sectionCcOptions = sectionCcOptions globalOptions ++ sectionCcOptions options
  , sectionCSources = sectionCSources globalOptions ++ sectionCSources options
  , sectionJsSources = sectionJsSources globalOptions ++ sectionJsSources options
  , sectionExtraLibDirs = sectionExtraLibDirs globalOptions ++ sectionExtraLibDirs options
  , sectionExtraLibraries = sectionExtraLibraries globalOptions ++ sectionExtraLibraries options
  , sectionExtraFrameworksDirs = sectionExtraFrameworksDirs globalOptions ++ sectionExtraFrameworksDirs options
  , sectionFrameworks = sectionFrameworks globalOptions ++ sectionFrameworks options
  , sectionIncludeDirs = sectionIncludeDirs globalOptions ++ sectionIncludeDirs options
  , sectionInstallIncludes = sectionInstallIncludes globalOptions ++ sectionInstallIncludes options
  , sectionLdOptions = sectionLdOptions globalOptions ++ sectionLdOptions options
  , sectionBuildable = sectionBuildable options <|> sectionBuildable globalOptions
  , sectionDependencies = sectionDependencies options <> sectionDependencies globalOptions
  , sectionConditionals = map (fmap (a <$)) (sectionConditionals globalOptions) ++ sectionConditionals options
  , sectionBuildTools = sectionBuildTools options <> sectionBuildTools globalOptions
  }

toSectionI :: Identity (WithCommonOptions a Identity CSources JsSources) -> Section a
toSectionI = toSection . runIdentity

toSection :: WithCommonOptions a Identity CSources JsSources -> Section a
toSection (Product CommonOptions{..} a) = Section {
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
      , sectionConditionals = conditionals
      , sectionBuildTools = fromMaybe mempty commonOptionsBuildTools
      }
  where
    conditionals = map toConditional (fromMaybeList commonOptionsWhen)

    toConditional :: ConditionalSection a Identity CSources JsSources -> Conditional (Section a)
    toConditional x = case x of
      FlatConditional (Identity (Product sect c)) -> Conditional (conditionCondition c) (toSection sect) Nothing
      ThenElseConditional (Identity (ThenElse condition then_ else_)) -> Conditional condition (toSectionI then_) (Just $ toSectionI else_)

pathsModuleFromPackageName :: String -> String
pathsModuleFromPackageName name = "Paths_" ++ map f name
  where
    f '-' = '_'
    f x = x

determineModules :: String -> [String] -> Maybe (List String) -> Maybe (List String) -> ([String], [String])
determineModules name modules mExposedModules mOtherModules = case (mExposedModules, mOtherModules) of
  (Nothing, Nothing) -> (modules, [pathsModuleFromPackageName name])
  _ -> (exposedModules, otherModules)
    where
      otherModules   = maybe ((modules \\ exposedModules) ++ pathsModule) fromList mOtherModules
      exposedModules = maybe (modules \\ otherModules)   fromList mExposedModules
      pathsModule = [pathsModuleFromPackageName name] \\ exposedModules

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

fromMaybeList :: Maybe (List a) -> [a]
fromMaybeList = maybe [] fromList
