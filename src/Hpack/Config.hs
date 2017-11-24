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
, toSectionM
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

data CommonOptions c a = CommonOptions {
  commonOptionsSourceDirs :: Maybe (List FilePath)
, commonOptionsDependencies :: Maybe Dependencies
, commonOptionsDefaultExtensions :: Maybe (List String)
, commonOptionsOtherExtensions :: Maybe (List String)
, commonOptionsGhcOptions :: Maybe (List GhcOption)
, commonOptionsGhcProfOptions :: Maybe (List GhcProfOption)
, commonOptionsGhcjsOptions :: Maybe (List GhcjsOption)
, commonOptionsCppOptions :: Maybe (List CppOption)
, commonOptionsCcOptions :: Maybe (List CcOption)
, commonOptionsCSources :: Maybe (List FilePath)
, commonOptionsJsSources :: Maybe (List FilePath)
, commonOptionsExtraLibDirs :: Maybe (List FilePath)
, commonOptionsExtraLibraries :: Maybe (List FilePath)
, commonOptionsExtraFrameworksDirs :: Maybe (List FilePath)
, commonOptionsFrameworks :: Maybe (List String)
, commonOptionsIncludeDirs :: Maybe (List FilePath)
, commonOptionsInstallIncludes :: Maybe (List FilePath)
, commonOptionsLdOptions :: Maybe (List LdOption)
, commonOptionsBuildable :: Maybe Bool
, commonOptionsWhen :: Maybe (List (ConditionalSection c a))
, commonOptionsBuildTools :: Maybe Dependencies
} deriving Generic

instance HasFieldNames (CommonOptions c a)

traverseCommonOptions :: (Monad m, Traversable d) => (forall b. c b -> m (d b)) -> CommonOptions c a -> m (CommonOptions d a)
traverseCommonOptions action c@CommonOptions{..} = do
  xs <- traverse (traverse (traverseConditionalSection action)) commonOptionsWhen
  return c {commonOptionsWhen = xs}

instance (FromJSON a, HasFieldNames a) => FromJSON (CommonOptions CaptureUnknownFields a) where
  parseJSON = genericParseJSON_

type WithCommonOptions c a = Product (CommonOptions c a) a

traverseWithCommonOptions :: (Monad m, Traversable d) => (forall b. c b -> m (d b)) -> WithCommonOptions c a -> m (WithCommonOptions d a)
traverseWithCommonOptions action = bitraverse (traverseCommonOptions action) return

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

data ConditionalSection c a =
    ThenElseConditional (c (ThenElse c a))
  | FlatConditional (c (Product (WithCommonOptions c a) Condition))

traverseConditionalSection :: (Monad m, Traversable d) =>
  (forall b. c b -> m (d b)) -> ConditionalSection c a -> m (ConditionalSection d a)
traverseConditionalSection action = \ case
  ThenElseConditional c -> ThenElseConditional <$> (action c >>= traverse (traverseThenElse action))
  FlatConditional c -> FlatConditional <$> (action c >>= traverse (bitraverse (traverseWithCommonOptions action) return))

instance (FromJSON a, HasFieldNames a) => FromJSON (ConditionalSection CaptureUnknownFields a) where
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

data ThenElse c a = ThenElse {
  _thenElseCondition :: String
, thenElseThen :: c (WithCommonOptions c a)
, thenElseElse :: c (WithCommonOptions c a)
} deriving Generic

traverseThenElse :: (Monad m, Traversable d) => (forall b. c b -> m (d b)) -> ThenElse c a -> m (ThenElse d a)
traverseThenElse action c@ThenElse{..} = do
  then_ <- action thenElseThen >>= traverse (traverseWithCommonOptions action)
  else_ <- action thenElseElse >>= traverse (traverseWithCommonOptions action)
  return c{thenElseThen = then_, thenElseElse = else_}

instance HasFieldNames (ThenElse c a)

instance (FromJSON a, HasFieldNames a) => FromJSON (ThenElse CaptureUnknownFields a) where
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

type LibraryConfig = CaptureUnknownFields (WithCommonOptions CaptureUnknownFields LibrarySection)
type ExecutableConfig = CaptureUnknownFields (WithCommonOptions CaptureUnknownFields ExecutableSection)

data PackageConfig = PackageConfig {
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
, packageConfigFlags :: Maybe (Map String (CaptureUnknownFields FlagSection))
, packageConfigExtraSourceFiles :: Maybe (List FilePath)
, packageConfigDataFiles :: Maybe (List FilePath)
, packageConfigGithub :: Maybe Text
, packageConfigGit :: Maybe String
, packageConfigCustomSetup :: Maybe (CaptureUnknownFields CustomSetupSection)
, packageConfigLibrary :: Maybe LibraryConfig
, packageConfigInternalLibraries :: Maybe (Map String LibraryConfig)
, packageConfigExecutable :: Maybe ExecutableConfig
, packageConfigExecutables :: Maybe (Map String ExecutableConfig)
, packageConfigTests :: Maybe (Map String ExecutableConfig)
, packageConfigBenchmarks :: Maybe (Map String ExecutableConfig)
} deriving Generic

instance HasFieldNames PackageConfig where
  ignoreUnderscoredUnknownFields _ = True

instance FromJSON PackageConfig where
  parseJSON value = handleNullValues <$> genericParseJSON_ value
    where
      handleNullValues :: PackageConfig -> PackageConfig
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

toSectionM :: Monad m => m (WithCommonOptions m a) -> m (Section a)
toSectionM = (>>= toSection)

toEmptySection :: Monad m => m (Product (CommonOptions m Empty) a) -> m (Section Empty, a)
toEmptySection x = x >>= \ (Product common a) -> do
  flip (,) a <$> toSection (Product common Empty)

type Config = CaptureUnknownFields (Product (CommonOptions CaptureUnknownFields Empty) PackageConfig)

toPackage :: FilePath -> Config -> IO ([String], Package)
toPackage dir c = do
  libraryResult <- mapM (toLibrary dir packageName_ globalOptions) mLibrarySection
  let
    executableWarnings :: [String]
    executableSections :: [(String, Section ExecutableSection)]
    (executableWarnings, executableSections) = (warnings, sections)
      where
        sections :: [(String, Section ExecutableSection)]
        sections = case mExecutable of
          Just executable -> [(packageName_, executable)]
          Nothing -> executables

        warnings = ignoringExecutablesWarning ++ unknownFieldWarningsExecutable ++ unknownFieldWarningsExecutables

        ignoringExecutablesWarning = case (packageConfigExecutable, packageConfigExecutables) of
          (Just _, Just _) -> ["Ignoring field \"executables\" in favor of \"executable\""]
          _ -> []

        (unknownFieldWarningsExecutables, executables) = toSections "executable" packageConfigExecutables
        (unknownFieldWarningsExecutable, mExecutable) = formatUnknownFieldsMaybe "executable section" (toSectionM <$> packageConfigExecutable)

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
           unknownPackageFieldWarnings
        ++ nameWarnings
        ++ flagWarnings
        ++ customSetupUnknownFieldsWarnings
        ++ libraryUnknownFieldsWarnings
        ++ internalLibrariesSectionsUnknownFieldsWarnings
        ++ testsSectionsUnknownFieldsWarnings
        ++ benchmarkSectionsUnknownFieldsWarnings
        ++ formatMissingSourceDirs missingSourceDirs
        ++ libraryWarnings
        ++ internalLibrariesWarnings
        ++ executableWarnings
        ++ executablesWarnings
        ++ testsWarnings
        ++ benchmarksWarnings
        ++ extraSourceFilesWarnings
        ++ dataFilesWarnings

  return (warnings, pkg)
  where
    (unknownPackageFieldWarnings, (globalOptions, PackageConfig{..})) =
      formatUnknownFields In "package description" (toEmptySection c)

    nameWarnings :: [String]
    packageName_ :: String
    (nameWarnings, packageName_) = case packageConfigName of
      Nothing -> let inferredName = takeBaseName dir in
        (["Package name not specified, inferred " ++ show inferredName], inferredName)
      Just n -> ([], n)

    mCustomSetup :: Maybe CustomSetup
    mCustomSetup = toCustomSetup <$> mCustomSetupSection

    (customSetupUnknownFieldsWarnings, mCustomSetupSection) =
      formatUnknownFieldsMaybe "custom-setup section" packageConfigCustomSetup

    (libraryUnknownFieldsWarnings, mLibrarySection) =
      formatUnknownFieldsMaybe "library section" (toSectionM <$> packageConfigLibrary)

    toSections :: String -> Maybe (Map String (CaptureUnknownFields (WithCommonOptions CaptureUnknownFields a))) -> ([String], [(String, Section a)])
    toSections sectionType sections = formatUnknownSectionFields sectionType (map (fmap toSectionM) $ toList sections)

    internalLibrariesSectionsUnknownFieldsWarnings :: [String]
    internalLibrariesSections :: [(String, Section LibrarySection)]
    (internalLibrariesSectionsUnknownFieldsWarnings, internalLibrariesSections) =
      toSections "internal-libraries" packageConfigInternalLibraries

    testsSectionsUnknownFieldsWarnings :: [String]
    testsSections :: [(String, Section ExecutableSection)]
    (testsSectionsUnknownFieldsWarnings, testsSections) = toSections "test" packageConfigTests

    benchmarkSectionsUnknownFieldsWarnings :: [String]
    benchmarkSections :: [(String, Section ExecutableSection)]
    (benchmarkSectionsUnknownFieldsWarnings, benchmarkSections) = toSections "benchmark" packageConfigBenchmarks

    (flagWarnings, flags) = second (map toFlag) xs
      where
        xs :: ([String], [(String, FlagSection)])
        xs = (formatUnknownFieldsList For "flag" . toList) packageConfigFlags

    toList :: Maybe (Map String a) -> [(String, a)]
    toList = Map.toList . fromMaybe mempty

    formatUnknownSectionFields :: String -> [(String, CaptureUnknownFields a)] -> ([String], [(String, a)])
    formatUnknownSectionFields sectionType = formatUnknownFieldsList In (sectionType ++ " section")

    formatUnknownFieldsList :: Preposition -> String -> [(String, CaptureUnknownFields a)] -> ([String], [(String, a)])
    formatUnknownFieldsList preposition bar = first concat . unzip . map f
      where
        f (name, fields) = (,) name <$> (formatUnknownFields preposition (bar ++ " " ++ show name) fields)

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

formatUnknownFieldsMaybe :: String -> Maybe (CaptureUnknownFields a) -> ([String], Maybe a)
formatUnknownFieldsMaybe name = maybe ([], Nothing) (fmap Just) . fmap (formatUnknownFields In name)

expandCSources :: FilePath -> Section a -> IO ([String], Section a)
expandCSources dir sect@Section{..} = do
  (warnings, files) <- expandGlobs "c-sources" dir sectionCSources
  return (warnings, sect {sectionCSources = files})

expandJsSources :: FilePath -> Section a -> IO ([String], Section a)
expandJsSources dir sect@Section{..} = do
  (warnings, files) <- expandGlobs "js-sources" dir sectionJsSources
  return (warnings, sect {sectionJsSources = files})

expandForeignSources :: FilePath -> Section a -> IO ([String], Section a)
expandForeignSources dir sect = do
  (cWarnings, sect_) <- expandCSources dir sect
  (jsWarnings, sect__) <- expandJsSources dir sect_
  return (cWarnings ++ jsWarnings, sect__)

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
  biTraverseSection fromLibrarySection fromLibrarySectionInConditional sect >>= expandForeignSources dir
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
  >=> expandForeignSources dir . nubOtherModules
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

toSection :: Monad m => WithCommonOptions m a -> m (Section a)
toSection = fmap toSectionIdentity . bitraverse (traverseCommonOptions $ fmap Identity) return

toSectionIdentity :: WithCommonOptions Identity a -> Section a
toSectionIdentity (Product CommonOptions{..} a) = Section {
        sectionData = a
      , sectionSourceDirs = fromMaybeList commonOptionsSourceDirs
      , sectionDefaultExtensions = fromMaybeList commonOptionsDefaultExtensions
      , sectionOtherExtensions = fromMaybeList commonOptionsOtherExtensions
      , sectionGhcOptions = fromMaybeList commonOptionsGhcOptions
      , sectionGhcProfOptions = fromMaybeList commonOptionsGhcProfOptions
      , sectionGhcjsOptions = fromMaybeList commonOptionsGhcjsOptions
      , sectionCppOptions = fromMaybeList commonOptionsCppOptions
      , sectionCcOptions = fromMaybeList commonOptionsCcOptions
      , sectionCSources = fromMaybeList commonOptionsCSources
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

toConditional :: ConditionalSection Identity a -> Conditional (Section a)
toConditional x = case x of
  FlatConditional (Identity (Product sect c)) -> Conditional (conditionCondition c) (toSectionIdentity sect) Nothing
  ThenElseConditional (Identity (ThenElse condition then_ else_)) -> Conditional condition (toSect then_) (Just $ toSect else_)
    where
      toSect = toSectionIdentity . runIdentity

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
