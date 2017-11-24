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

data CommonOptions c cSources a = CommonOptions {
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
, commonOptionsJsSources :: Maybe (List FilePath)
, commonOptionsExtraLibDirs :: Maybe (List FilePath)
, commonOptionsExtraLibraries :: Maybe (List FilePath)
, commonOptionsExtraFrameworksDirs :: Maybe (List FilePath)
, commonOptionsFrameworks :: Maybe (List String)
, commonOptionsIncludeDirs :: Maybe (List FilePath)
, commonOptionsInstallIncludes :: Maybe (List FilePath)
, commonOptionsLdOptions :: Maybe (List LdOption)
, commonOptionsBuildable :: Maybe Bool
, commonOptionsWhen :: Maybe (List (ConditionalSection c cSources a))
, commonOptionsBuildTools :: Maybe Dependencies
} deriving Generic

traverseCommonOptionsCSources :: (Monad m, Traversable c) => (x -> m y) -> CommonOptions c x a -> m (CommonOptions c y a)
traverseCommonOptionsCSources action c@CommonOptions{..} = do
  r <- action commonOptionsCSources
  xs <- traverse (traverse (traverseConditionalSectionCSources action)) commonOptionsWhen
  return c{commonOptionsCSources = r, commonOptionsWhen = xs}

instance HasFieldNames (ParseCommonOptions a)

traverseCommonOptions :: (Monad m, Traversable d) => (forall b. c b -> m (d b)) -> CommonOptions c cSources a -> m (CommonOptions d cSources a)
traverseCommonOptions action c@CommonOptions{..} = do
  xs <- traverse (traverse (traverseConditionalSection action)) commonOptionsWhen
  return c {commonOptionsWhen = xs}

type ParseCSources = Maybe (List FilePath)

type ParseCommonOptions = CommonOptions CaptureUnknownFields ParseCSources

instance (FromJSON a, HasFieldNames a) => FromJSON (ParseCommonOptions a) where
  parseJSON = genericParseJSON_

type WithCommonOptions c cSources a = Product (CommonOptions c cSources a) a

traverseWithCommonOptions :: (Monad m, Traversable d) => (forall b. c b -> m (d b)) -> WithCommonOptions c cSources a -> m (WithCommonOptions d cSources a)
traverseWithCommonOptions action = bitraverse (traverseCommonOptions action) return

traverseWithCommonOptionsCSources :: (Monad m, Traversable c) => (x -> m y) -> WithCommonOptions c x a -> m (WithCommonOptions c y a)
traverseWithCommonOptionsCSources action = bitraverse (traverseCommonOptionsCSources action) return

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

data ConditionalSection c cSources a =
    ThenElseConditional (c (ThenElse c cSources a))
  | FlatConditional (c (Product (WithCommonOptions c cSources a) Condition))

traverseConditionalSectionCSources :: (Monad m, Traversable c) => (x -> m y) -> ConditionalSection c x a -> m (ConditionalSection c y a)
traverseConditionalSectionCSources action = \ case
  ThenElseConditional c -> ThenElseConditional <$> traverse (traverseThenElseCSources action) c
  FlatConditional c -> FlatConditional <$> traverse (bitraverse (traverseWithCommonOptionsCSources action) return) c

traverseThenElseCSources :: (Monad m, Traversable c) => (x -> m y) -> ThenElse c x a -> m (ThenElse c y a)
traverseThenElseCSources action c@ThenElse{..} = do
  then_ <- traverse (traverseWithCommonOptionsCSources action) thenElseThen
  else_ <- traverse (traverseWithCommonOptionsCSources action) thenElseElse
  return c{thenElseThen = then_, thenElseElse = else_}

traverseConditionalSection :: (Monad m, Traversable d) =>
  (forall b. c b -> m (d b)) -> ConditionalSection c cSources a -> m (ConditionalSection d cSources a)
traverseConditionalSection action = \ case
  ThenElseConditional c -> ThenElseConditional <$> (action c >>= traverse (traverseThenElse action))
  FlatConditional c -> FlatConditional <$> (action c >>= traverse (bitraverse (traverseWithCommonOptions action) return))

type ParseConditionalSection = ConditionalSection CaptureUnknownFields ParseCSources

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

data ThenElse c cSources a = ThenElse {
  _thenElseCondition :: String
, thenElseThen :: c (WithCommonOptions c cSources a)
, thenElseElse :: c (WithCommonOptions c cSources a)
} deriving Generic

traverseThenElse :: (Monad m, Traversable d) => (forall b. c b -> m (d b)) -> ThenElse c cSources a -> m (ThenElse d cSources a)
traverseThenElse action c@ThenElse{..} = do
  then_ <- action thenElseThen >>= traverse (traverseWithCommonOptions action)
  else_ <- action thenElseElse >>= traverse (traverseWithCommonOptions action)
  return c{thenElseThen = then_, thenElseElse = else_}

type ParseThenElse = ThenElse CaptureUnknownFields ParseCSources

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

type LibraryConfig m cSources = m (WithCommonOptions m cSources LibrarySection)
type ExecutableConfig m cSources = m (WithCommonOptions m cSources ExecutableSection)

data PackageConfig m cSources = PackageConfig {
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
, packageConfigFlags :: Maybe (Map String (m FlagSection))
, packageConfigExtraSourceFiles :: Maybe (List FilePath)
, packageConfigDataFiles :: Maybe (List FilePath)
, packageConfigGithub :: Maybe Text
, packageConfigGit :: Maybe String
, packageConfigCustomSetup :: Maybe (m CustomSetupSection)
, packageConfigLibrary :: Maybe (LibraryConfig m cSources)
, packageConfigInternalLibraries :: Maybe (Map String (LibraryConfig m cSources))
, packageConfigExecutable :: Maybe (ExecutableConfig m cSources)
, packageConfigExecutables :: Maybe (Map String (ExecutableConfig m cSources))
, packageConfigTests :: Maybe (Map String (ExecutableConfig m cSources))
, packageConfigBenchmarks :: Maybe (Map String (ExecutableConfig m cSources))
} deriving Generic

type ParsePackageConfig = PackageConfig CaptureUnknownFields ParseCSources

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

type ParseConfig = CaptureUnknownFields (Product (CommonOptions CaptureUnknownFields ParseCSources Empty) ParsePackageConfig)

toPackage :: FilePath -> ParseConfig -> IO ([String], Package)
toPackage dir c = do
  let (c_, xs) = (extractUnknownFieldWarnings c)
  c__ <- expandCSources dir c_
  toPackage_ dir (fmap (++ xs) c__)

toPackage_ :: FilePath
  -> ((WithCommonOptions Identity CSources Empty, PackageConfig Identity CSources), [String])
  -> IO ([String], Package)
toPackage_ dir ((toSection -> globalOptions, PackageConfig{..}), packageWarnings) = do
  libraryResult <- mapM (toLibrary dir packageName_ globalOptions) mLibrarySection

  let
    executableSections :: [(String, Section ExecutableSection)]
    (executableWarning, executableSections) = (warning, sections)
      where
        sections :: [(String, Section ExecutableSection)]
        sections = case mExecutable of
          Just executable -> [(packageName_, executable)]
          Nothing -> executables

        warning = case mExecutable of
          Just _ | not (null executables) -> ["Ignoring field \"executables\" in favor of \"executable\""]
          _ -> []

        executables = toSections packageConfigExecutables

        mExecutable :: Maybe (Section ExecutableSection)
        mExecutable = toSectionI <$> packageConfigExecutable

    mLibrary :: Maybe (Section Library)
    mLibrary = fmap snd libraryResult

    libraryWarnings :: [String]
    libraryWarnings = maybe [] fst libraryResult

  (internalLibrariesWarnings, internalLibraries) <- toInternalLibraries dir packageName_ globalOptions internalLibrariesSections
  (executablesWarnings, executables) <- toExecutables dir packageName_ globalOptions executableSections
  (testsWarnings, tests) <- toExecutables dir packageName_ globalOptions testsSections
  (benchmarksWarnings, benchmarks) <- toExecutables dir packageName_ globalOptions benchmarkSections

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
        ++ libraryWarnings
        ++ internalLibrariesWarnings
        ++ executableWarning
        ++ executablesWarnings
        ++ testsWarnings
        ++ benchmarksWarnings
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

    toSections :: Maybe (Map String (Identity (WithCommonOptions Identity CSources a))) -> [(String, Section a)]
    toSections = map (fmap toSectionI) . toList

    internalLibrariesSections :: [(String, Section LibrarySection)]
    internalLibrariesSections = toSections packageConfigInternalLibraries

    testsSections :: [(String, Section ExecutableSection)]
    testsSections = toSections packageConfigTests

    benchmarkSections :: [(String, Section ExecutableSection)]
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

type WarningsIO = WriterT [FilePath] IO

type CSources = [FilePath]

expandCSources :: FilePath
  -> (WithCommonOptions Identity ParseCSources Empty, PackageConfig Identity ParseCSources)
  -> IO ((WithCommonOptions Identity CSources Empty, PackageConfig Identity CSources), [String])
expandCSources dir = runWriterT . bitraverse expand expandPackageConfig
  where
    expandPackageConfig :: Traversable c => PackageConfig c ParseCSources -> WarningsIO (PackageConfig c CSources)
    expandPackageConfig p@PackageConfig{..} = do
      library <- traverse (traverse expand) packageConfigLibrary
      internalLibraries <- traverse (traverse (traverse expand)) packageConfigInternalLibraries
      executable <- traverse (traverse expand) packageConfigExecutable
      executables <- traverse (traverse (traverse expand)) packageConfigExecutables
      tests <- traverse (traverse (traverse expand)) packageConfigTests
      benchmarks <- traverse (traverse (traverse expand)) packageConfigBenchmarks
      return p {
          packageConfigLibrary = library
        , packageConfigInternalLibraries = internalLibraries
        , packageConfigExecutable = executable
        , packageConfigExecutables = executables
        , packageConfigTests = tests
        , packageConfigBenchmarks = benchmarks
        }

    expand :: Traversable c => WithCommonOptions c ParseCSources a -> WarningsIO (WithCommonOptions c CSources a)
    expand = bitraverse expandCSources_ return

    expandCSources_ :: Traversable c => CommonOptions c ParseCSources a -> WarningsIO (CommonOptions c [FilePath] a)
    expandCSources_ = traverseCommonOptionsCSources (f. fromMaybeList)
      where
        f :: [FilePath] -> WarningsIO [FilePath]
        f xs = do
          (warnings, files) <- liftIO $ expandGlobs "c-sources" dir xs
          tell warnings
          return files

type Warnings = Writer [String]

extractUnknownFieldWarnings :: ParseConfig -> ((WithCommonOptions Identity ParseCSources Empty, PackageConfig Identity ParseCSources), [String])
extractUnknownFieldWarnings c = runWriter $ do
  Identity (Product globalOptions p) <- warnUnknownFields In "package description" (c >>= sequenceCommonOptions)
  (,) (Product globalOptions Empty) <$> warnPackage p
  where
    sequenceCommonOptions :: Monad m => Product (CommonOptions m c a) b -> m (Product (CommonOptions Identity c a) b)
    sequenceCommonOptions = bitraverse (traverseCommonOptions $ fmap Identity) return

    warnPackage :: ParsePackageConfig -> Warnings (PackageConfig Identity ParseCSources)
    warnPackage p@PackageConfig{..} = do
      flags <- (warnUnknownFieldsList For "flag" . toList) packageConfigFlags
      customSetup <- warnUnknownFields In "custom-setup section" (sequence packageConfigCustomSetup)
      library <- warnUnknownFields In "library section" (traverse (>>= sequenceCommonOptions) packageConfigLibrary)
      internalLibraries <- warnUnknownSectionFields "internal-libraries" packageConfigInternalLibraries
      executable <- warnUnknownFields In "executable section" (traverse (>>= sequenceCommonOptions) packageConfigExecutable)
      executables <- warnUnknownSectionFields "executable" packageConfigExecutables
      tests <- warnUnknownSectionFields "test" packageConfigTests
      benchmarks <- warnUnknownSectionFields "benchmark" packageConfigBenchmarks
      return p {
          packageConfigFlags = fromList flags
        , packageConfigCustomSetup = sequence customSetup
        , packageConfigLibrary = sequence library
        , packageConfigInternalLibraries = internalLibraries
        , packageConfigExecutable = sequence executable
        , packageConfigExecutables = executables
        , packageConfigTests = tests
        , packageConfigBenchmarks = benchmarks
        }

    warnUnknownFields :: Preposition -> String -> CaptureUnknownFields a -> Warnings (Identity a)
    warnUnknownFields preposition name x = do
      let (warnings, y) = formatUnknownFields preposition name x
      tell warnings
      return (Identity y)

    warnUnknownSectionFields sectionType = fmap fromList . warnUnknownFieldsList In (sectionType ++ " section") . map (fmap (>>= sequenceCommonOptions)) . toList

    warnUnknownFieldsList :: Preposition -> String -> [(String, CaptureUnknownFields a)] -> Warnings [(String, Identity a)]
    warnUnknownFieldsList preposition sect = mapM f
      where
        f (name, fields) = (,) name <$> (warnUnknownFields preposition (sect ++ " " ++ show name) fields)

    toList :: Maybe (Map String a) -> [(String, a)]
    toList = Map.toList . fromMaybe mempty

    fromList :: [(String, a)] -> Maybe (Map String a)
    fromList = Just . Map.fromList

expandJsSources :: FilePath -> Section a -> IO ([String], Section a)
expandJsSources dir sect@Section{..} = do
  (warnings, files) <- expandGlobs "js-sources" dir sectionJsSources
  return (warnings, sect {sectionJsSources = files})

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

toLibrary :: FilePath -> String -> Section global -> Section LibrarySection -> IO ([String], Section Library)
toLibrary dir name globalOptions library =
  biTraverseSection fromLibrarySection fromLibrarySectionInConditional sect >>= expandJsSources dir
  where
    sect :: Section LibrarySection
    sect = mergeSections emptyLibrarySection globalOptions library

    fromLibrarySection :: Section LibrarySection -> IO Library
    fromLibrarySection Section{sectionData = LibrarySection{..}, sectionSourceDirs = sourceDirs} = do
      modules <- concat <$> mapM (getModules dir) sourceDirs
      let (exposedModules, otherModules) = determineModules name modules librarySectionExposedModules librarySectionOtherModules
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

nubOtherModules :: Section Executable -> Section Executable
nubOtherModules = mapSectionAcc f []
  where
    f :: [String] -> Executable -> ([String], Executable)
    f outerOtherModules executable = (
        outerOtherModules ++ otherModules
      , executable {executableOtherModules = otherModules}
      )
      where
        otherModules = executableOtherModules executable \\ outerOtherModules

mapSectionAcc :: (acc -> a -> (acc, b)) -> acc -> Section a -> Section b
mapSectionAcc f = go
  where
    go acc0 sect@Section{..} = let (acc, b) = f acc0 sectionData in sect {
        sectionData = b
      , sectionConditionals = map (go acc <$>) sectionConditionals
      }

toInternalLibraries :: FilePath -> String -> Section global -> [(String, Section LibrarySection)] -> IO ([String], Map String (Section Library))
toInternalLibraries dir packageName_ globalOptions = traverseNamedSections (toLibrary dir packageName_ globalOptions)

traverseNamedSections :: (Ord name, Monad m) => (a -> m ([warning], b)) -> [(name, a)] -> m ([warning], Map name b)
traverseNamedSections f namedSections = do
  result <- mapM f sections
  let (warnings, xs) = unzip result
  return (concat warnings, Map.fromList $ zip names xs)
  where
    names = map fst namedSections
    sections = map snd namedSections

toExecutables :: FilePath -> String -> Section global -> [(String, Section ExecutableSection)] -> IO ([String], Map String (Section Executable))
toExecutables dir packageName_ globalOptions = traverseNamedSections (toExecutable dir packageName_ globalOptions)

toExecutable :: FilePath -> String -> Section global -> Section ExecutableSection -> IO ([String], Section Executable)
toExecutable dir packageName_ globalOptions =
      toExecutable_ . mergeSections emptyExecutableSection globalOptions
  >=> expandJsSources dir . nubOtherModules
  where
    toExecutable_ :: Section ExecutableSection -> IO (Section Executable)
    toExecutable_ sect@Section{..} = do
      (executable, ghcOptions) <- fromExecutableSection sectionData
      conditionals <- mapM (traverse toExecutable_) sectionConditionals
      return sect {
          sectionData = executable
        , sectionGhcOptions = sectionGhcOptions ++ ghcOptions
        , sectionConditionals = conditionals
        }
      where
        fromExecutableSection :: ExecutableSection -> IO (Executable, [GhcOption])
        fromExecutableSection ExecutableSection{..} = do
          modules <- maybe inferModules (return . fromList) executableSectionOtherModules
          return (Executable mainSrcFile modules, ghcOptions)
          where
            inferModules :: IO [String]
            inferModules
              | null sectionSourceDirs = return []
              | otherwise = filterMain . (++ [pathsModule]) . concat <$> mapM (getModules dir) sectionSourceDirs

            pathsModule = pathsModuleFromPackageName packageName_

            filterMain :: [String] -> [String]
            filterMain = maybe id (maybe id (filter . (/=)) . toModule . splitDirectories) executableSectionMain

            (mainSrcFile, ghcOptions) = maybe (Nothing, []) (first Just . parseMain) executableSectionMain

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

toSectionI :: Identity (WithCommonOptions Identity CSources a) -> Section a
toSectionI = toSection . runIdentity

toSection :: WithCommonOptions Identity CSources a -> Section a
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
      , sectionJsSources = fromMaybeList commonOptionsJsSources
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

    toConditional :: ConditionalSection Identity CSources a -> Conditional (Section a)
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
