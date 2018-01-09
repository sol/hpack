{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
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
, pathsModuleFromPackageName
, determineModules
, BuildType(..)
, Cond(..)

, LibrarySection(..)
, fromLibrarySectionInConditional
#endif
) where

import           Control.Applicative
import           Control.Monad
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
import           System.Directory
import           System.FilePath
import           Data.Functor.Identity
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Writer
import           Control.Monad.Trans.Except
import           Control.Monad.IO.Class

import           Hpack.Syntax.Util
import           Hpack.Syntax.UnknownFields
import           Hpack.Syntax
import           Hpack.Util hiding (expandGlobs)
import qualified Hpack.Util as Util
import           Hpack.Defaults
import qualified Hpack.Yaml as Yaml
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
  , packageExtraDocFiles = []
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
section a = Section a [] mempty [] [] [] [] [] [] [] [] [] [] [] [] [] [] [] [] [] Nothing [] mempty

packageConfig :: FilePath
packageConfig = "package.yaml"

data CustomSetupSection = CustomSetupSection {
  customSetupSectionDependencies :: Maybe Dependencies
} deriving (Eq, Show, Generic)

instance HasFieldNames CustomSetupSection

instance FromJSON CustomSetupSection where
  parseJSON = genericParseJSON

data LibrarySection = LibrarySection {
  librarySectionExposed :: Maybe Bool
, librarySectionExposedModules :: Maybe (List String)
, librarySectionOtherModules :: Maybe (List String)
, librarySectionReexportedModules :: Maybe (List String)
, librarySectionSignatures :: Maybe (List String)
} deriving (Eq, Show, Generic)

emptyLibrarySection :: LibrarySection
emptyLibrarySection = LibrarySection Nothing Nothing Nothing Nothing Nothing

instance HasFieldNames LibrarySection

instance FromJSON LibrarySection where
  parseJSON = genericParseJSON

data ExecutableSection = ExecutableSection {
  executableSectionMain :: Maybe FilePath
, executableSectionOtherModules :: Maybe (List String)
} deriving (Eq, Show, Generic)

emptyExecutableSection :: ExecutableSection
emptyExecutableSection = ExecutableSection Nothing Nothing

instance HasFieldNames ExecutableSection

instance FromJSON ExecutableSection where
  parseJSON = genericParseJSON

data CommonOptions capture cSources jsSources a = CommonOptions {
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
, commonOptionsJsSources :: jsSources
, commonOptionsExtraLibDirs :: Maybe (List FilePath)
, commonOptionsExtraLibraries :: Maybe (List FilePath)
, commonOptionsExtraFrameworksDirs :: Maybe (List FilePath)
, commonOptionsFrameworks :: Maybe (List String)
, commonOptionsIncludeDirs :: Maybe (List FilePath)
, commonOptionsInstallIncludes :: Maybe (List FilePath)
, commonOptionsLdOptions :: Maybe (List LdOption)
, commonOptionsBuildable :: Maybe Bool
, commonOptionsWhen :: Maybe (List (ConditionalSection capture cSources jsSources a))
, commonOptionsBuildTools :: Maybe Dependencies
} deriving (Functor, Generic)

instance (Monoid cSources, Monoid jsSources) => Monoid (CommonOptions capture cSources jsSources a) where
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
  }
  mappend a b = CommonOptions {
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
  , commonOptionsBuildTools = commonOptionsBuildTools b <> commonOptionsBuildTools a
  }

type ParseCommonOptions = CommonOptions CaptureUnknownFields ParseCSources ParseJsSources

instance HasFieldNames (ParseCommonOptions a)

instance (FromJSON a, HasFieldNames a) => FromJSON (ParseCommonOptions a) where
  parseJSON = genericParseJSON

type ParseCSources = Maybe (List FilePath)
type ParseJsSources = Maybe (List FilePath)

type CSources = [FilePath]
type JsSources = [FilePath]

type WithCommonOptions capture cSources jsSources a = Product (CommonOptions capture cSources jsSources a) a

data Traverse m capture capture_ cSources cSources_ jsSources jsSources_ = Traverse {
  traverseCapture :: forall a. capture a -> m (capture_ a)
, traverseCSources :: cSources -> m cSources_
, traverseJsSources :: jsSources -> m jsSources_
}

defaultTraverse :: Applicative m => Traverse m capture capture cSources cSources jsSources jsSources
defaultTraverse = Traverse {
  traverseCapture = pure
, traverseCSources = pure
, traverseJsSources = pure
}

type Traversal t = forall m capture capture_ cSources cSources_ jsSources jsSources_. (Monad m, Traversable capture_)
  => Traverse m capture capture_ cSources cSources_ jsSources jsSources_
  -> t capture cSources jsSources
  -> m (t capture_ cSources_ jsSources_)

type Traversal_ t = forall m capture capture_ cSources cSources_ jsSources jsSources_ a. (Monad m, Traversable capture_)
  => Traverse m capture capture_ cSources cSources_ jsSources jsSources_
  -> t capture cSources jsSources a
  -> m (t capture_ cSources_ jsSources_ a)

traverseCommonOptions :: Traversal_ CommonOptions
traverseCommonOptions t@Traverse{..} c@CommonOptions{..} = do
  cSources <- traverseCSources commonOptionsCSources
  jsSources <- traverseJsSources commonOptionsJsSources
  xs <- traverse (traverse (traverseConditionalSection t)) commonOptionsWhen
  return c {
      commonOptionsCSources = cSources
    , commonOptionsJsSources = jsSources
    , commonOptionsWhen = xs
    }

traverseConditionalSection :: Traversal_ ConditionalSection
traverseConditionalSection t@Traverse{..} = \ case
  ThenElseConditional c -> ThenElseConditional <$> (traverseCapture c >>= traverse (bitraverse (traverseThenElse t) return))
  FlatConditional c -> FlatConditional <$> (traverseCapture c >>= traverse (bitraverse (traverseWithCommonOptions t) return))

traverseThenElse :: Traversal_ ThenElse
traverseThenElse t@Traverse{..} c@ThenElse{..} = do
  then_ <- traverseCapture thenElseThen >>= traverse (traverseWithCommonOptions t)
  else_ <- traverseCapture thenElseElse >>= traverse (traverseWithCommonOptions t)
  return c{thenElseThen = then_, thenElseElse = else_}

traverseWithCommonOptions :: Traversal_ WithCommonOptions
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

data ConditionalSection capture cSources jsSources a =
    ThenElseConditional (capture (Product (ThenElse capture cSources jsSources a) Condition))
  | FlatConditional (capture (Product (WithCommonOptions capture cSources jsSources a) Condition))

instance Functor capture => Functor (ConditionalSection capture cSources jsSources) where
  fmap f = \ case
    ThenElseConditional c -> ThenElseConditional (first (fmap f) <$> c)
    FlatConditional c -> FlatConditional (first (bimap (fmap f) f) <$> c)

type ParseConditionalSection = ConditionalSection CaptureUnknownFields ParseCSources ParseJsSources

instance (FromJSON a, HasFieldNames a) => FromJSON (ParseConditionalSection a) where
  parseJSON v
    | hasKey "then" v || hasKey "else" v = ThenElseConditional <$> parseJSON v
    | otherwise = FlatConditional <$> parseJSON v

hasKey :: Text -> Value -> Bool
hasKey key (Object o) = HashMap.member key o
hasKey _ _ = False

newtype Condition = Condition {
  _conditionCondition :: Cond
} deriving (Eq, Show, Generic)

instance FromJSON Condition where
  parseJSON = genericParseJSON

instance HasFieldNames Condition

newtype Cond = Cond String
  deriving (Eq, Show)

instance FromJSON Cond where
  parseJSON v = case v of
    String _ -> Cond <$> parseJSON v
    Bool True -> return (Cond "true")
    Bool False -> return (Cond "false")
    _ -> typeMismatch "Boolean or String" v

data ThenElse capture cSources jsSources a = ThenElse {
  thenElseThen :: capture (WithCommonOptions capture cSources jsSources a)
, thenElseElse :: capture (WithCommonOptions capture cSources jsSources a)
} deriving Generic

instance Functor capture => Functor (ThenElse capture cSources jsSources) where
  fmap f c@ThenElse{..} = c{thenElseThen = map_ thenElseThen, thenElseElse = map_ thenElseElse}
    where
      map_ = fmap (bimap (fmap f) f)

type ParseThenElse = ThenElse CaptureUnknownFields ParseCSources ParseJsSources

instance HasFieldNames (ParseThenElse a)

instance (FromJSON a, HasFieldNames a) => FromJSON (ParseThenElse a) where
  parseJSON = genericParseJSON

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

type SectionConfig capture cSources jsSources a = capture (WithCommonOptions capture cSources jsSources a)

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
, packageConfigExtraDocFiles :: Maybe (List FilePath)
, packageConfigDataFiles :: Maybe (List FilePath)
, packageConfigGithub :: Maybe Text
, packageConfigGit :: Maybe String
, packageConfigCustomSetup :: Maybe (capture CustomSetupSection)
, packageConfigLibrary :: Maybe (SectionConfig capture cSources jsSources LibrarySection)
, packageConfigInternalLibraries :: Maybe (Map String (SectionConfig capture cSources jsSources LibrarySection))
, packageConfigExecutable :: Maybe (SectionConfig capture cSources jsSources ExecutableSection)
, packageConfigExecutables :: Maybe (Map String (SectionConfig capture cSources jsSources ExecutableSection))
, packageConfigTests :: Maybe (Map String (SectionConfig capture cSources jsSources ExecutableSection))
, packageConfigBenchmarks :: Maybe (Map String (SectionConfig capture cSources jsSources ExecutableSection))
, packageConfigDefaults :: Maybe (List (capture Defaults))
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
  defaults <- traverse (traverse traverseCapture) packageConfigDefaults
  return p {
      packageConfigFlags = flags
    , packageConfigCustomSetup = customSetup
    , packageConfigLibrary = library
    , packageConfigInternalLibraries = internalLibraries
    , packageConfigExecutable = executable
    , packageConfigExecutables = executables
    , packageConfigTests = tests
    , packageConfigBenchmarks = benchmarks
    , packageConfigDefaults = defaults
    }
  where
    traverseNamedConfigs = traverse . traverse . traverseSectionConfig

traverseSectionConfig :: Traversal_ SectionConfig
traverseSectionConfig t = traverseCapture t >=> traverse (traverseWithCommonOptions t)

type ParsePackageConfig = PackageConfig CaptureUnknownFields ParseCSources ParseJsSources

instance HasFieldNames ParsePackageConfig where
  ignoreUnderscoredUnknownFields _ = True

instance FromJSON ParsePackageConfig where
  parseJSON value = handleNullValues <$> genericParseJSON value
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

type Warnings m = WriterT [String] m
type Errors = ExceptT String

decodeYaml :: FromJSON a => FilePath -> Warnings (Errors IO) a
decodeYaml = lift . ExceptT . Yaml.decodeYaml

readPackageConfig :: FilePath -> FilePath -> IO (Either String (Package, [String]))
readPackageConfig userDataDir file = runExceptT $ runWriterT $ do
  config <- decodeYaml file
  dir <- liftIO $ takeDirectory <$> canonicalizePath file
  toPackage userDataDir dir config

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
, packageExtraDocFiles :: [FilePath]
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
, librarySignatures :: [String]
} deriving (Eq, Show)

data Executable = Executable {
  executableMain :: Maybe FilePath
, executableOtherModules :: [String]
} deriving (Eq, Show)

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
  parseJSON = genericParseJSON

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
  Product (CommonOptions capture cSources jsSources Empty) (PackageConfig capture cSources jsSources)

traverseConfig :: Traversal Config
traverseConfig t = bitraverse (traverseCommonOptions t) (traversePackageConfig t)

type ParseConfig = CaptureUnknownFields (Config CaptureUnknownFields ParseCSources ParseJsSources)

toPackage :: FilePath -> FilePath -> ParseConfig -> Warnings (Errors IO) Package
toPackage userDataDir dir =
      warnUnknownFieldsInConfig
  >=> expandDefaults userDataDir
  >=> traverseConfig (expandForeignSources dir)
  >=> toPackage_ dir

expandDefaults
  :: FilePath
  -> Config Identity ParseCSources ParseJsSources
  -> Warnings (Errors IO) (Config Identity ParseCSources ParseJsSources)
expandDefaults userDataDir (Product global config) = do
  d <- getDefaults userDataDir config
  return (Product (d <> global) config)

getDefaults
  :: FilePath
  -> PackageConfig Identity cSources jsSources
  -> Warnings (Errors IO) (CommonOptions Identity ParseCSources ParseJsSources Empty)
getDefaults userDataDir PackageConfig{..} = do
  mconcat <$> mapM go (fromMaybeList packageConfigDefaults)
  where
    go (runIdentity -> defaults) = do
      file <- lift $ ExceptT (ensure userDataDir defaults)
      decodeYaml file >>= warnUnknownFieldsInDefaults file

toExecutableMap :: Monad m => String -> Maybe (Map String a) -> Maybe a -> Warnings m (Maybe (Map String a))
toExecutableMap name executables mExecutable = do
  case mExecutable of
    Just executable -> do
      when (isJust executables) $ do
        tell ["Ignoring field \"executables\" in favor of \"executable\""]
      return $ Just (Map.fromList [(name, executable)])
    Nothing -> return executables

type GlobalOptions = CommonOptions Identity CSources JsSources Empty

toPackage_ :: MonadIO m => FilePath -> Product GlobalOptions (PackageConfig Identity CSources JsSources) -> Warnings m Package
toPackage_ dir (Product globalOptions PackageConfig{..}) = do
  mLibrary <- liftIO $ traverse (toLibrary dir packageName_ globalOptions) packageConfigLibrary

  internalLibraries <- liftIO $ toInternalLibraries dir packageName_ globalOptions packageConfigInternalLibraries

  executables <- toExecutableMap packageName_ packageConfigExecutables packageConfigExecutable
    >>= liftIO . toExecutables dir packageName_ globalOptions

  tests <- liftIO $ toExecutables dir packageName_ globalOptions packageConfigTests
  benchmarks <- liftIO $ toExecutables dir packageName_ globalOptions packageConfigBenchmarks

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
  dataFiles <- expandGlobs "data-files" dir (fromMaybeList packageConfigDataFiles)

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
      , packageExtraDocFiles = extraDocFiles
      , packageDataFiles = dataFiles
      , packageSourceRepository = sourceRepository
      , packageCustomSetup = mCustomSetup
      , packageLibrary = mLibrary
      , packageInternalLibraries = internalLibraries
      , packageExecutables = executables
      , packageTests = tests
      , packageBenchmarks = benchmarks
      }

  tell nameWarnings
  tell (formatMissingSourceDirs missingSourceDirs)
  return pkg
  where
    nameWarnings :: [String]
    packageName_ :: String
    (nameWarnings, packageName_) = case packageConfigName of
      Nothing -> let inferredName = takeBaseName dir in
        (["Package name not specified, inferred " ++ show inferredName], inferredName)
      Just n -> ([], n)

    mCustomSetup :: Maybe CustomSetup
    mCustomSetup = toCustomSetup . runIdentity <$> packageConfigCustomSetup

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

sequenceUnknownFields :: Monad capture => Traverse capture capture Identity cSources cSources jsSources jsSources
sequenceUnknownFields = defaultTraverse{traverseCapture = fmap Identity}

warnUnknownFieldsInDefaults
  :: Monad m
  => String
  -> CaptureUnknownFields (CommonOptions CaptureUnknownFields cSources jsSources Empty)
  -> Warnings m (CommonOptions Identity cSources jsSources Empty)
warnUnknownFieldsInDefaults name = warnUnknownFields In name . (>>= traverseCommonOptions sequenceUnknownFields)

warnUnknownFieldsInConfig :: forall m. Monad m => ParseConfig -> Warnings m (Config Identity ParseCSources ParseJsSources)
warnUnknownFieldsInConfig = warnGlobal >=> bitraverse return warnSections
  where
    t = sequenceUnknownFields

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
      defaults <- warnUnknownFields In "defaults section" (traverse (traverse $ traverseCapture t) packageConfigDefaults)
      return p {
          packageConfigFlags = flags
        , packageConfigCustomSetup = customSetup
        , packageConfigLibrary = library
        , packageConfigInternalLibraries = internalLibraries
        , packageConfigExecutable = executable
        , packageConfigExecutables = executables
        , packageConfigTests = tests
        , packageConfigBenchmarks = benchmarks
        , packageConfigDefaults = defaults
        }

    warnNamedSection
      :: String
      -> Maybe (Map String (SectionConfig CaptureUnknownFields cSources jsSources a))
      -> Warnings m (Maybe (Map String (SectionConfig Identity cSources jsSources a)))
    warnNamedSection sectionType = traverse (warnNamed In (sectionType ++ " section") . fmap (traverseSectionConfig t))

    warnNamed :: Preposition -> String -> Map String (CaptureUnknownFields a) -> Warnings m (Map String a)
    warnNamed preposition sect = fmap Map.fromList . mapM f . Map.toList
      where
        f (name, fields) = (,) name <$> (warnUnknownFields preposition (sect ++ " " ++ show name) fields)

warnUnknownFields :: forall m a. Monad m => Preposition -> String -> CaptureUnknownFields a -> Warnings m a
warnUnknownFields preposition name = fmap snd . bitraverse tell return . formatUnknownFields preposition name

expandForeignSources
  :: MonadIO m
  => FilePath
  -> Traverse (Warnings m) capture capture ParseCSources CSources ParseJsSources JsSources
expandForeignSources dir = defaultTraverse {
    traverseCSources = expand "c-sources"
  , traverseJsSources = expand "js-sources"
  }
  where
    expand fieldName xs = do
      expandGlobs fieldName dir (fromMaybeList xs)

expandGlobs :: MonadIO m => String -> FilePath -> [String] -> Warnings m [FilePath]
expandGlobs name dir patterns = do
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
getMentionedLibraryModules LibrarySection{..} =
  fromMaybeList librarySectionExposedModules ++ fromMaybeList librarySectionOtherModules

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

toLibrary :: FilePath -> String -> GlobalOptions -> SectionConfig Identity CSources JsSources LibrarySection -> IO (Section Library)
toLibrary dir name globalOptions =
    inferModules dir name getMentionedLibraryModules getLibraryModules fromLibrarySectionTopLevel fromLibrarySectionInConditional
  . toSectionI (emptyLibrarySection <$ globalOptions)
  where
    getLibraryModules :: Library -> [String]
    getLibraryModules Library{..} = libraryExposedModules ++ libraryOtherModules

    fromLibrarySectionTopLevel pathsModule inferableModules LibrarySection{..} =
      Library librarySectionExposed exposedModules otherModules reexportedModules signatures
      where
        (exposedModules, otherModules) =
          determineModules pathsModule inferableModules librarySectionExposedModules librarySectionOtherModules
        reexportedModules = fromMaybeList librarySectionReexportedModules
        signatures = fromMaybeList librarySectionSignatures

determineModules :: [String] -> [String] -> Maybe (List String) -> Maybe (List String) -> ([String], [String])
determineModules pathsModule inferableModules mExposedModules mOtherModules = case (mExposedModules, mOtherModules) of
  (Nothing, Nothing) -> (inferableModules, pathsModule)
  _ -> (exposedModules, otherModules)
    where
      exposedModules = maybe (inferableModules \\ otherModules) fromList mExposedModules
      otherModules   = maybe ((inferableModules ++ pathsModule) \\ exposedModules) fromList mOtherModules

fromLibrarySectionInConditional :: [String] -> LibrarySection -> Library
fromLibrarySectionInConditional inferableModules lib@(LibrarySection _ exposedModules otherModules _ _) = do
  case (exposedModules, otherModules) of
    (Nothing, Nothing) -> (fromLibrarySectionPlain lib) {libraryOtherModules = inferableModules}
    _ -> fromLibrarySectionPlain lib

fromLibrarySectionPlain :: LibrarySection -> Library
fromLibrarySectionPlain LibrarySection{..} = Library {
    libraryExposed = librarySectionExposed
  , libraryExposedModules = fromMaybeList librarySectionExposedModules
  , libraryOtherModules = fromMaybeList librarySectionOtherModules
  , libraryReexportedModules = fromMaybeList librarySectionReexportedModules
  , librarySignatures = fromMaybeList librarySectionSignatures
  }

toInternalLibraries :: FilePath -> String -> GlobalOptions -> Maybe (Map String (SectionConfig Identity CSources JsSources LibrarySection)) -> IO (Map String (Section Library))
toInternalLibraries dir packageName_ globalOptions = traverse (toLibrary dir packageName_ globalOptions) . fromMaybe mempty

toExecutables :: FilePath -> String -> GlobalOptions -> Maybe (Map String (SectionConfig Identity CSources JsSources ExecutableSection)) -> IO (Map String (Section Executable))
toExecutables dir packageName_ globalOptions = traverse (toExecutable dir packageName_ globalOptions) . fromMaybe mempty

getMentionedExecutableModules :: ExecutableSection -> [String]
getMentionedExecutableModules ExecutableSection{..} =
  fromMaybeList executableSectionOtherModules ++ maybe [] return (executableSectionMain >>= toModule . splitDirectories)

toExecutable :: FilePath -> String -> GlobalOptions -> SectionConfig Identity CSources JsSources ExecutableSection -> IO (Section Executable)
toExecutable dir packageName_ globalOptions =
    inferModules dir packageName_ getMentionedExecutableModules executableOtherModules fromExecutableSection (fromExecutableSection [])
  . expandMain
  . toSectionI (emptyExecutableSection <$ globalOptions)
  where
    fromExecutableSection :: [String] -> [String] -> ExecutableSection -> Executable
    fromExecutableSection pathsModule inferableModules ExecutableSection{..} =
      (Executable executableSectionMain otherModules)
      where
        otherModules = maybe (inferableModules ++ pathsModule) fromList executableSectionOtherModules

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

toSectionI :: CommonOptions Identity CSources JsSources a -> Identity (WithCommonOptions Identity CSources JsSources a) -> Section a
toSectionI globalOptions = toSection globalOptions . runIdentity

toSection :: CommonOptions Identity CSources JsSources a -> WithCommonOptions Identity CSources JsSources a -> Section a
toSection globalOptions (Product options a) = toSection_ (Product (globalOptions <> options) a)

toSection_ :: WithCommonOptions Identity CSources JsSources a -> Section a
toSection_ (Product CommonOptions{..} a) = Section {
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
      , sectionPkgConfigDependencies = fromMaybeList commonOptionsPkgConfigDependencies
      , sectionConditionals = conditionals
      , sectionBuildTools = fromMaybe mempty commonOptionsBuildTools
      }
  where
    toSectionI_ :: Identity (WithCommonOptions Identity CSources JsSources a) -> Section a
    toSectionI_ = toSection_ . runIdentity

    conditionals = map toConditional (fromMaybeList commonOptionsWhen)

    toConditional :: ConditionalSection Identity CSources JsSources a -> Conditional (Section a)
    toConditional x = case x of
      FlatConditional (Identity (Product sect c)) -> conditional c (toSection_ sect) Nothing
      ThenElseConditional (Identity (Product (ThenElse then_ else_) c)) -> conditional c (toSectionI_ then_) (Just $ toSectionI_ else_)
      where
        conditional (Condition (Cond c)) = Conditional c

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

fromMaybeList :: Maybe (List a) -> [a]
fromMaybeList = maybe [] fromList
