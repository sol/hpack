{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}
module Hpack.Config (
  packageConfig
, readPackageConfig
, encodePackage
, writePackage
, renamePackage
, packageDependencies
, package
, section
, Package(..)
, Dependency(..)
, AddSource(..)
, GitUrl
, GitRef
, GhcOption
, Section(..)
, Library(..)
, Executable(..)
, Conditional(..)
, Flag(..)
, SourceRepository(..)
#ifdef TEST
, renameDependencies
, HasFieldNames(..)
, CaptureUnknownFields(..)
, Empty(..)
, getModules
, determineModules
#endif
) where

import           Control.Applicative
import           Control.Monad.Compat
import           Data.Aeson
import           Data.Aeson.Types
import           Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import           Data.Data
import           Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import qualified Data.HashMap.Lazy as HashMap
import           Data.List.Compat (elemIndex, intersect, isPrefixOf, nub,
                                   sortBy, (\\))
import           Data.Maybe
import           Data.Ord
import           Data.String
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as Vector
import qualified Data.Yaml.Pretty as Yaml
import           GHC.Generics (Generic, Rep)
import           Prelude ()
import           Prelude.Compat
import           System.Directory
import           System.FilePath

import           Hpack.GenericsUtil
import           Hpack.Util
import           Hpack.Yaml

package :: String -> String -> Package
package name version = Package name version Nothing Nothing Nothing Nothing Nothing Nothing [] [] [] Nothing Nothing Nothing [] [] [] Nothing Nothing [] [] []

renamePackage :: String -> Package -> Package
renamePackage name p@Package{..} = p {
    packageName = name
  , packageExecutables = map (renameDependencies packageName name) packageExecutables
  , packageTests = map (renameDependencies packageName name) packageTests
  , packageBenchmarks = map (renameDependencies packageName name) packageBenchmarks
  }

renameDependencies :: String -> String -> Section a -> Section a
renameDependencies old new sect@Section{..} = sect {sectionDependencies = map rename sectionDependencies, sectionConditionals = map renameConditional sectionConditionals}
  where
    rename dep
      | dependencyName dep == old = dep {dependencyName = new}
      | otherwise = dep

    renameConditional :: Conditional -> Conditional
    renameConditional (Conditional condition then_ else_) = Conditional condition (renameDependencies old new then_) (renameDependencies old new <$> else_)

packageDependencies :: Package -> [Dependency]
packageDependencies Package{..} = nub . sortBy (comparing (lexicographically . dependencyName)) $
     (concatMap sectionDependencies packageExecutables)
  ++ (concatMap sectionDependencies packageTests)
  ++ (concatMap sectionDependencies packageBenchmarks)
  ++ maybe [] sectionDependencies packageLibrary

section :: a -> Section a
section a = Section a [] [] [] [] [] [] [] [] [] [] [] [] [] [] Nothing [] []

packageConfig :: FilePath
packageConfig = "package.yaml"

githubBaseUrl :: String
githubBaseUrl = "https://github.com/"

jsonOptions :: String -> Options
jsonOptions name = defaultOptions { fieldLabelModifier = hyphenize name
                                  , omitNothingFields = True
                                  }

genericToJSON_ :: forall a. (Generic a, GToJSON (Rep a), HasTypeName a) => a -> Value
genericToJSON_ =
    removeEmptyObjects .
    removeEmptyArrays .
    genericToJSON (jsonOptions name)
  where
    name :: String
    name = typeName (Proxy :: Proxy a)

removeEmptyObjects :: Value -> Value
removeEmptyObjects (Object o) = Object $ HashMap.filter (/= Object mempty) o
removeEmptyObjects v = v

removeEmptyArrays :: Value -> Value
removeEmptyArrays (Object o) = Object $ HashMap.filter (/= Array mempty) o
removeEmptyArrays v = v

genericParseJSON_ :: forall a. (Generic a, GFromJSON (Rep a), HasTypeName a) => Value -> Parser a
genericParseJSON_ = genericParseJSON (jsonOptions name)
  where
    name :: String
    name = typeName (Proxy :: Proxy a)

hyphenize :: String -> String -> String
hyphenize name =
#if MIN_VERSION_aeson(0,10,0)
  camelTo2
#else
  camelTo
#endif
  '-' . drop (length name) . dropWhile (== '_')

type FieldName = String

class HasFieldNames a where
  fieldNames :: Proxy a -> [FieldName]

  default fieldNames :: (HasTypeName a, Selectors (Rep a)) => Proxy a -> [String]
  fieldNames proxy = map (hyphenize $ typeName proxy) (selectors proxy)

data CaptureUnknownFields a = CaptureUnknownFields {
  captureUnknownFieldsFields :: [FieldName]
, captureUnknownFieldsValue :: a
} deriving (Eq, Show, Generic)

captureUnknownFields :: forall a. (HasFieldNames a, FromJSON a) => Value -> Parser (CaptureUnknownFields a)
captureUnknownFields v = CaptureUnknownFields unknown <$> parseJSON v
  where
    unknown = getUnknownFields v (Proxy :: Proxy a)

instance (HasFieldNames a, FromJSON a) => FromJSON (CaptureUnknownFields (Section a)) where
  parseJSON v = do
    (unknownFields, sect) <- toSection <$> parseJSON v <*> parseJSON v
    return (CaptureUnknownFields (unknownSectionFields ++ unknownFields) sect)
    where
      unknownSectionFields = getUnknownFields v (Proxy :: Proxy (Section a))

instance FromJSON (CaptureUnknownFields FlagSection) where
  parseJSON = captureUnknownFields

getUnknownFields :: forall a. HasFieldNames a => Value -> Proxy a -> [FieldName]
getUnknownFields v _ = case v of
  Object o -> unknown
    where
      unknown = keys \\ fields
      keys = map T.unpack (HashMap.keys o)
      fields = fieldNames (Proxy :: Proxy a)
  _ -> []

data LibrarySection = LibrarySection {
  librarySectionExposed :: Maybe Bool
, librarySectionExposedModules :: Maybe (List String)
, librarySectionOtherModules :: Maybe (List String)
, librarySectionReexportedModules :: Maybe (List String)
} deriving (Eq, Show, Generic)

instance HasFieldNames LibrarySection

instance FromJSON LibrarySection where
  parseJSON = genericParseJSON_

data ExecutableSection = ExecutableSection {
  executableSectionMain :: FilePath
, executableSectionOtherModules :: Maybe (List String)
} deriving (Eq, Show, Generic)

instance HasFieldNames ExecutableSection

instance FromJSON ExecutableSection where
  parseJSON = genericParseJSON_

data CommonOptions = CommonOptions {
  commonOptionsSourceDirs :: Maybe (List FilePath)
, commonOptionsDependencies :: Maybe (List Dependency)
, commonOptionsDefaultExtensions :: Maybe (List String)
, commonOptionsOtherExtensions :: Maybe (List String)
, commonOptionsGhcOptions :: Maybe (List GhcOption)
, commonOptionsGhcProfOptions :: Maybe (List GhcProfOption)
, commonOptionsCppOptions :: Maybe (List CppOption)
, commonOptionsCCOptions :: Maybe (List CCOption)
, commonOptionsCSources :: Maybe (List FilePath)
, commonOptionsExtraLibDirs :: Maybe (List FilePath)
, commonOptionsExtraLibraries :: Maybe (List FilePath)
, commonOptionsIncludeDirs :: Maybe (List FilePath)
, commonOptionsInstallIncludes :: Maybe (List FilePath)
, commonOptionsLdOptions :: Maybe (List LdOption)
, commonOptionsBuildable :: Maybe Bool
, commonOptionsWhen :: Maybe (List ConditionalSection)
, commonOptionsBuildTools :: Maybe (List Dependency)
} deriving (Eq, Show, Generic)

instance HasFieldNames CommonOptions

instance FromJSON CommonOptions where
  parseJSON = genericParseJSON_

data ConditionalSection = ThenElseConditional (CaptureUnknownFields ThenElse) | FlatConditional (CaptureUnknownFields (Section Condition))
  deriving (Eq, Show, Generic)

instance FromJSON ConditionalSection where
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

data ThenElse = ThenElse {
  _thenElseCondition :: String
, _thenElseThen :: (CaptureUnknownFields (Section Empty))
, _thenElseElse :: (CaptureUnknownFields (Section Empty))
} deriving (Eq, Show, Generic)

instance FromJSON (CaptureUnknownFields ThenElse) where
  parseJSON = captureUnknownFields

instance HasFieldNames ThenElse

instance FromJSON ThenElse where
  parseJSON = genericParseJSON_

data Empty = Empty
  deriving (Eq, Show, Generic)

instance FromJSON Empty where
  parseJSON _ = return Empty

instance HasFieldNames Empty where
  fieldNames _ = []

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
, packageConfigLicense :: Maybe String
, packageConfigLicenseFile :: Maybe String
, packageConfigTestedWith :: Maybe String
, packageConfigFlags :: Maybe (Map String (CaptureUnknownFields FlagSection))
, packageConfigExtraSourceFiles :: Maybe (List FilePath)
, packageConfigDataFiles :: Maybe (List FilePath)
, packageConfigGithub :: Maybe Text
, packageConfigGit :: Maybe String
, packageConfigLibrary :: Maybe (CaptureUnknownFields (Section LibrarySection))
, packageConfigExecutables :: Maybe (Map String (CaptureUnknownFields (Section ExecutableSection)))
, packageConfigTests :: Maybe (Map String (CaptureUnknownFields (Section ExecutableSection)))
, packageConfigBenchmarks :: Maybe (Map String (CaptureUnknownFields (Section ExecutableSection)))
} deriving (Eq, Show, Generic)

instance HasFieldNames PackageConfig

instance ToJSON Package where
  toJSON p =
      removeEmptyArrays $
      removeEmptyObjects $
      (\(Object o) -> Object $ case packageSourceRepository p of
              Just repo ->
                  let srepo = (sourceRepositoryUrl repo) ++
                          (fromMaybe "" (sourceRepositorySubdir repo))
                  in
                  if githubBaseUrl `isPrefixOf` srepo
                     then let dropIfGH (Just (String v))
                                  | githubBaseUrl `isPrefixOf` T.unpack v = Nothing
                              dropIfGH v = v
                          in
                          HashMap.insert "github" (String (T.pack (drop (length githubBaseUrl) srepo))) $
                          HashMap.alter dropIfGH "bug-reports" $
                          HashMap.alter dropIfGH "homepage" o
                     else HashMap.insert "git" (String (T.pack srepo)) o
              Nothing -> o
      ) $
      (\(Object o) -> Object $ HashMap.delete "source-repository" o) $
      (\(Object o) -> Object $ HashMap.mapWithKey convertSingletons o) $
      (\(Object o) ->
           Object $
           HashMap.alter
           (\l -> case l of
                      Just "LICENSE" -> Nothing
                      _ -> l)
           "license-file"
           o
      ) $
      pullCommonFields "build-tools" $
      pullCommonFields "conditionals" $
      pullCommonFields "buildable" $
      pullCommonFields "ld-options" $
      pullCommonFields "install-includes" $
      pullCommonFields "include-dirs" $
      pullCommonFields "extra-libraries" $
      pullCommonFields "extra-lib-dirs" $
      pullCommonFields "c-sources" $
      pullCommonFields "cc-options" $
      pullCommonFields "cpp-options" $
      pullCommonFields "ghc-prof-options" $
      pullCommonFields "ghc-options" $
      pullCommonFields "other-extensions" $
      pullCommonFields "default-extensions" $
      pullCommonFields "other-modules" $
      pullCommonFields "source-dirs" $
      pullCommonFields "dependencies" (genericToJSON_ p)

pullCommonFields :: Text -> Value -> Value
pullCommonFields field topLevel@(Object topLevelObj) =
    let commonField = let deps = mapMaybe getField [ "library"
                                                   , "executables"
                                                   , "benchmarks"
                                                   , "tests"
                                                   ]
                      in maybe [] (\h -> foldl intersect h deps) (listToMaybe deps)
    in mergeObjects (Object (filterCommon commonField)) $
       mergeObjects topLevel (object [ field .= commonField ])
  where
    filterCommon :: [Value] -> HashMap.HashMap Text Value
    filterCommon commonField =
        let helper :: Maybe Value -> Maybe Value
            helper (Just (Array vs)) =
                let v = Vector.filter (not . (`elem` commonField)) vs
                in if Vector.null v then Nothing else Just (Array v)
            helper Nothing = Nothing
            helper (Just v) = Just v
            outerHelper = (\(Object sectObj) -> Object $ HashMap.alter helper field sectObj)
            outermostHelper = (\(Object e) -> Object $ HashMap.map outerHelper e)
            o' = HashMap.adjust outerHelper "library" topLevelObj
            o'' = HashMap.adjust outermostHelper "executables" o'
            o''' = HashMap.adjust outermostHelper "benchmarks" o''
            o'''' = HashMap.adjust outermostHelper "tests" o'''
        in o''''
    getField "library" =
        case (HashMap.lookup "library" topLevelObj >>= unObject) of
            Nothing -> Nothing
            Just lib -> do
                return $ fromMaybe [] (HashMap.lookup field lib >>= unArray)
    getField name =
        case (HashMap.lookup name topLevelObj >>= unObject) of
            Nothing -> Nothing
            Just sect -> do
                blocks <- mapM unObject (map snd (HashMap.toList sect))
                return $ concat $ mapMaybe (HashMap.lookup field >=> unArray) blocks
    unArray (Array v) = Just (Vector.toList v)
    unArray _ = Nothing
    unObject (Object o) = Just o
    unObject _ = Nothing
pullCommonFields _ v = v

omitBuildableTrue :: Value -> Value
omitBuildableTrue (Object o) = Object (HashMap.filterWithKey f o)
  where
    f "buildable" (Bool True) = False
    f _ _ = True
omitBuildableTrue v = v

omitSection :: Value -> Value
omitSection (Object o) = Object $
    HashMap.mapWithKey convertSingletons $
    HashMap.filterWithKey omitSectionEntry o
omitSection v = v

convertSingletons :: Text -> Value -> Value
convertSingletons "ghc-options" (Array a) = convertSingleton a
convertSingletons "cpp-options" (Array a) = convertSingleton a
convertSingletons "cc-options" (Array a) = convertSingleton a
convertSingletons "c-sources" (Array a) = convertSingleton a
convertSingletons "ld-options" (Array a) = convertSingleton a
convertSingletons "ghc-prof-options" (Array a) = convertSingleton a
convertSingletons "extra-lib-dirs" (Array a) = convertSingleton a
convertSingletons "extra-libraries" (Array a) = convertSingleton a
convertSingletons "copyright" (Array a) = convertSingleton a
convertSingletons "maintainer" (Array a) = convertSingleton a
convertSingletons "author" (Array a) = convertSingleton a
convertSingletons "source-dirs" (Array a) = convertSingleton a
convertSingletons _ v = v

convertSingleton :: Vector.Vector Value -> Value
convertSingleton a =
    if Vector.length a == 1
    then Vector.head a
    else Array a

omitSectionEntry :: Text -> Value -> Bool
omitSectionEntry "license-file" "LICENSE" = False
omitSectionEntry "data" _ =  False
omitSectionEntry "conditionals" _ =  False
omitSectionEntry "name" _ =  False
omitSectionEntry "exposed" (Bool True) =  False
omitSectionEntry "other-modules" _ =  False
omitSectionEntry _ _ =  True

mergeObjects :: Value -> Value -> Value
mergeObjects (Object o1) (Object o2) = Object (o1 `mappend` o2)
mergeObjects (Object o1) _ = Object o1
mergeObjects _ (Object o2) = Object o2
mergeObjects v _ = v

instance ToJSON [Section Executable] where
  toJSON ss = Object $
      HashMap.fromList $ map helper ss
    where
      helper sect@Section{..} = ( T.pack (executableName sectionData)
                                , toJSON sect
                                )

instance {-# OVERLAPS #-} ToJSON (Section ()) where
  toJSON sect@Section{..} =
    (omitSection
      (mergeObjects
       (mergeObjects
        (genericToJSON_ sect)
        (toJSON sectionData))
       (object $ case sectionConditionals of
          [] -> []
          cs -> case toJSON (omitRedundantBuildables cs) of
            Array [] -> []
            Array csObjs -> case Vector.filter (/= Object mempty) csObjs of
              [] -> []
              csV -> ["when" .= csV]
            v -> ["when" .= v])))
    where
      omitRedundantBuildables = map $ \(Conditional c i e) ->
          Conditional c (omitRedundantBuildable i) (omitRedundantBuildable <$> e)
      omitRedundantBuildable s
        | fromMaybe True sectionBuildable ==
          fromMaybe True (Hpack.Config.sectionBuildable s) =
          s {sectionBuildable = Nothing}
      omitRedundantBuildable s = s

instance (Generic (Section a), GToJSON (Rep (Section a)), HasTypeName (Section a),
          ToJSON a) => ToJSON (Section a) where
  toJSON sect@Section{..} =
    omitBuildableTrue (omitSection
      (mergeObjects
       (mergeObjects
        (genericToJSON_ sect)
        (toJSON sectionData))
       (object $ case sectionConditionals of
          [] -> []
          cs -> case toJSON (omitRedundantBuildables cs) of
            Array [] -> []
            Array csObjs -> case Vector.filter (/= Object mempty) csObjs of
              [] -> []
              csV -> ["when" .= csV]
            csArr -> ["when" .= csArr])))
    where
      omitRedundantBuildables = map $ \(Conditional c i e) ->
          Conditional c (omitRedundantBuildable i) (omitRedundantBuildable <$> e)
      omitRedundantBuildable s
        | fromMaybe True sectionBuildable ==
          fromMaybe True (Hpack.Config.sectionBuildable s) =
          s {sectionBuildable = Nothing}
      omitRedundantBuildable s = s

instance ToJSON Conditional where
  toJSON (Conditional cnd ifSection Nothing) = case toJSON ifSection of
    -- If an empty block is generated strip it out
    Object [] -> object []
    ifSectionObj -> mergeObjects (object [ "condition" .= toJSON cnd ]) ifSectionObj
  toJSON (Conditional cnd ifSection (Just elseSection)) = case toJSON ifSection of
    -- If an empty block is at the if statement negate the condition
    Object [] -> toJSON (Conditional ("!(" ++ cnd ++ ")") elseSection Nothing)
    ifSectionObj -> case toJSON elseSection of
      Object [] -> toJSON (Conditional cnd ifSection Nothing)
      elseSectionObj ->
        object [ "condition" .= toJSON cnd
               , "then" .= ifSectionObj
               , "else" .= elseSectionObj
               ]

instance ToJSON AddSource where
  toJSON = genericToJSON_

instance ToJSON Dependency where
  toJSON (Dependency d Nothing) = fromString d
  toJSON (Dependency d (Just ref)) =
      object ([ "name" .= d
              ] `mappend` case toJSON ref of
                              Object ps -> HashMap.toList ps
                              _ -> mempty)

instance ToJSON Executable where
  toJSON = genericToJSON_

instance ToJSON Library where
  toJSON = genericToJSON_

instance ToJSON [Flag] where
  toJSON fs = Object $
      HashMap.fromList $ map helper fs
    where
      helper Flag{..} = ( T.pack flagName
                        , object [ "description" .= toJSON flagDescription
                                 , "manual" .= toJSON flagManual
                                 , "default" .= toJSON flagDefault
                                 ]
                        )

instance ToJSON SourceRepository where
  toJSON = genericToJSON_

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

encodePackage :: Package -> ByteString
encodePackage pkg = Yaml.encodePretty config pkg
  where
    config = Yaml.setConfCompare keyWeight Yaml.defConfig
    keys = [ "condition"
           , "then"
           , "else"
           , "name"
           , "version"
           , "synopsis"
           , "description"
           , "category"
           , "author"
           , "maintainer"
           , "copyright"
           , "license"
           , "license-file"
           , "github"
           , "homepage"
           , "git"
           , "bug-reports"
           , "main"
           , "source-dirs"
           , "extra-source-files"
           , "c-sources"
           , "default-extensions"
           , "other-extensions"
           , "ghc-options"
           , "ghc-prof-options"
           , "cc-options"
           , "cpp-options"
           , "ld-options"
           , "extra-lib-dirs"
           , "extra-libraries"
           , "include-dirs"
           , "install-includes"
           , "build-tools"
           , "exposed-modules"
           , "dependencies"
           , "buildable"
           , "when"
           , "library"
           , "executables"
           , "tests"
           , "benchmarks"
           ]
    keyWeight k1 k2 =
        fromMaybe maxBound (elemIndex k1 keys)
        `compare`
        fromMaybe maxBound (elemIndex k2 keys)

writePackage :: FilePath -> Package -> IO ()
writePackage fp pkg = ByteString.writeFile fp (encodePackage pkg)

readPackageConfig :: FilePath -> IO (Either String ([String], Package))
readPackageConfig file = do
  r <- decodeYaml file
  case r of
    Left err -> return (Left err)
    Right config -> do
      dir <- takeDirectory <$> canonicalizePath file
      Right <$> mkPackage dir config

data Dependency = Dependency {
  dependencyName :: String
, dependencyGitRef :: Maybe AddSource
} deriving (Eq, Show, Ord, Generic)

instance IsString Dependency where
  fromString name = Dependency name Nothing

instance FromJSON Dependency where
  parseJSON v = case v of
    String _ -> fromString <$> parseJSON v
    Object o -> addSourceDependency o
    _ -> typeMismatch "String or an Object" v
    where
      addSourceDependency o = Dependency <$> name <*> (Just <$> (local <|> git))
        where
          name :: Parser String
          name = o .: "name"

          local :: Parser AddSource
          local = Local <$> o .: "path"

          git :: Parser AddSource
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

data AddSource = GitRef GitUrl GitRef (Maybe FilePath) | Local FilePath
  deriving (Eq, Show, Ord, Generic)

type GitUrl = String
type GitRef = String

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
, packageLicense :: Maybe String
, packageLicenseFile :: Maybe FilePath
, packageTestedWith :: Maybe String
, packageFlags :: [Flag]
, packageExtraSourceFiles :: [FilePath]
, packageDataFiles :: [FilePath]
, packageSourceRepository :: Maybe SourceRepository
, packageLibrary :: Maybe (Section Library)
, packageExecutables :: [Section Executable]
, packageTests :: [Section Executable]
, packageBenchmarks :: [Section Executable]
} deriving (Eq, Show, Generic)

data Library = Library {
  libraryExposed :: Maybe Bool
, libraryExposedModules :: [String]
, libraryOtherModules :: [String]
, libraryReexportedModules :: [String]
} deriving (Eq, Show, Generic)

data Executable = Executable {
  executableName :: String
, executableMain :: FilePath
, executableOtherModules :: [String]
} deriving (Eq, Show, Generic)

data Section a = Section {
  sectionData :: a
, sectionSourceDirs :: [FilePath]
, sectionDependencies :: [Dependency]
, sectionDefaultExtensions :: [String]
, sectionOtherExtensions :: [String]
, sectionGhcOptions :: [GhcOption]
, sectionGhcProfOptions :: [GhcProfOption]
, sectionCppOptions :: [CppOption]
, sectionCCOptions :: [CCOption]
, sectionCSources :: [FilePath]
, sectionExtraLibDirs :: [FilePath]
, sectionExtraLibraries :: [FilePath]
, sectionIncludeDirs :: [FilePath]
, sectionInstallIncludes :: [FilePath]
, sectionLdOptions :: [LdOption]
, sectionBuildable :: Maybe Bool
, sectionConditionals :: [Conditional]
, sectionBuildTools :: [Dependency]
} deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

data Conditional = Conditional {
  conditionalCondition :: String
, conditionalThen :: Section ()
, conditionalElse :: Maybe (Section ())
} deriving (Eq, Show, Generic)

instance HasFieldNames a => HasFieldNames (Section a) where
  fieldNames Proxy = fieldNames (Proxy :: Proxy a) ++ fieldNames (Proxy :: Proxy CommonOptions)

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
} deriving (Eq, Show, Generic)

toFlag :: (String, FlagSection) -> Flag
toFlag (name, FlagSection description manual def) = Flag name description manual def

data SourceRepository = SourceRepository {
  sourceRepositoryUrl :: String
, sourceRepositorySubdir :: Maybe String
} deriving (Eq, Show, Generic)

mkPackage :: FilePath -> (CaptureUnknownFields (Section PackageConfig)) -> IO ([String], Package)
mkPackage dir (CaptureUnknownFields unknownFields globalOptions@Section{sectionData = PackageConfig{..}}) = do
  let name = fromMaybe (takeBaseName dir) packageConfigName

  mLibrary <- mapM (toLibrary dir name globalOptions) mLibrarySection
  executables <- toExecutables dir globalOptions (map (fmap captureUnknownFieldsValue) executableSections)
  tests <- toExecutables dir globalOptions (map (fmap captureUnknownFieldsValue) testsSections)
  benchmarks <- toExecutables dir globalOptions  (map (fmap captureUnknownFieldsValue) benchmarkSections)

  licenseFileExists <- doesFileExist (dir </> "LICENSE")

  missingSourceDirs <- nub . sort <$> filterM (fmap not <$> doesDirectoryExist . (dir </>)) (
       maybe [] sectionSourceDirs mLibrary
    ++ concatMap sectionSourceDirs executables
    ++ concatMap sectionSourceDirs tests
    ++ concatMap sectionSourceDirs benchmarks
    )

  (extraSourceFilesWarnings, extraSourceFiles) <-
    expandGlobs dir (fromMaybeList packageConfigExtraSourceFiles)

  (dataFilesWarnings, dataFiles) <-
    expandGlobs dir (fromMaybeList packageConfigDataFiles)

  let pkg = Package {
        packageName = name
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
      , packageLicense = packageConfigLicense
      , packageLicenseFile = packageConfigLicenseFile <|> (guard licenseFileExists >> Just "LICENSE")
      , packageTestedWith = packageConfigTestedWith
      , packageFlags = flags
      , packageExtraSourceFiles = extraSourceFiles
      , packageDataFiles = dataFiles
      , packageSourceRepository = sourceRepository
      , packageLibrary = mLibrary
      , packageExecutables = executables
      , packageTests = tests
      , packageBenchmarks = benchmarks
      }

      warnings =
           formatUnknownFields "package description" unknownFields
        ++ flagWarnings
        ++ maybe [] (formatUnknownFields "library section") (captureUnknownFieldsFields <$> packageConfigLibrary)
        ++ formatUnknownSectionFields "executable" executableSections
        ++ formatUnknownSectionFields "test" testsSections
        ++ formatMissingSourceDirs missingSourceDirs
        ++ extraSourceFilesWarnings
        ++ dataFilesWarnings

  return (warnings, pkg)
  where
    executableSections :: [(String, CaptureUnknownFields (Section ExecutableSection))]
    executableSections = toList packageConfigExecutables

    testsSections :: [(String, CaptureUnknownFields (Section ExecutableSection))]
    testsSections = toList packageConfigTests

    benchmarkSections :: [(String, CaptureUnknownFields (Section ExecutableSection))]
    benchmarkSections = toList packageConfigBenchmarks

    (flagWarnings, flags) = (concatMap formatUnknownFlagFields xs, map (toFlag . fmap captureUnknownFieldsValue) xs)
      where
        xs :: [(String, CaptureUnknownFields FlagSection)]
        xs = toList packageConfigFlags

        formatUnknownFlagFields :: (String, CaptureUnknownFields a) -> [String]
        formatUnknownFlagFields (name, fields) = map f (captureUnknownFieldsFields fields)
          where f field = "Ignoring unknown field " ++ show field ++ " for flag " ++ show name

    toList :: Maybe (Map String a) -> [(String, a)]
    toList = Map.toList . fromMaybe mempty

    mLibrarySection :: Maybe (Section LibrarySection)
    mLibrarySection = captureUnknownFieldsValue <$> packageConfigLibrary

    formatUnknownFields :: String -> [FieldName] -> [String]
    formatUnknownFields name = map f . sort
      where
        f field = "Ignoring unknown field " ++ show field ++ " in " ++ name

    formatUnknownSectionFields :: String -> [(String, CaptureUnknownFields a)] -> [String]
    formatUnknownSectionFields sectionType = concatMap f . map (fmap captureUnknownFieldsFields)
      where
        f :: (String, [String]) -> [String]
        f (sect, fields) = formatUnknownFields (sectionType ++ " section " ++ show sect) fields

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

toLibrary :: FilePath -> String -> Section global -> Section LibrarySection -> IO (Section Library)
toLibrary dir name globalOptions library = traverse fromLibrarySection sect
  where
    sect :: Section LibrarySection
    sect = mergeSections globalOptions library

    sourceDirs :: [FilePath]
    sourceDirs = sectionSourceDirs sect

    fromLibrarySection :: LibrarySection -> IO Library
    fromLibrarySection LibrarySection{..} = do
      modules <- concat <$> mapM (getModules dir) sourceDirs
      let (exposedModules, otherModules) = determineModules name modules librarySectionExposedModules librarySectionOtherModules
          reexportedModules = fromMaybeList librarySectionReexportedModules
      return (Library librarySectionExposed exposedModules otherModules reexportedModules)

toExecutables :: FilePath -> Section global -> [(String, Section ExecutableSection)] -> IO [Section Executable]
toExecutables dir globalOptions executables = mapM toExecutable sections
  where
    sections :: [(String, Section ExecutableSection)]
    sections = map (fmap $ mergeSections globalOptions) executables

    toExecutable :: (String, Section ExecutableSection) -> IO (Section Executable)
    toExecutable (name, sect@Section{..}) = do
      (executable, ghcOptions) <- fromExecutableSection sectionData
      return (sect {sectionData = executable, sectionGhcOptions = sectionGhcOptions ++ ghcOptions})
      where
        fromExecutableSection :: ExecutableSection -> IO (Executable, [GhcOption])
        fromExecutableSection ExecutableSection{..} = do
          modules <- maybe (filterMain . concat <$> mapM (getModules dir) sectionSourceDirs) (return . fromList) executableSectionOtherModules
          return (Executable name mainSrcFile modules, ghcOptions)
          where
            filterMain :: [String] -> [String]
            filterMain = maybe id (filter . (/=)) (toModule $ splitDirectories executableSectionMain)

            (mainSrcFile, ghcOptions) = parseMain executableSectionMain

mergeSections :: Section global -> Section a -> Section a
mergeSections globalOptions options
  = Section {
    sectionData = sectionData options
  , sectionSourceDirs = sectionSourceDirs globalOptions ++ sectionSourceDirs options
  , sectionDefaultExtensions = sectionDefaultExtensions globalOptions ++ sectionDefaultExtensions options
  , sectionOtherExtensions = sectionOtherExtensions globalOptions ++ sectionOtherExtensions options
  , sectionGhcOptions = sectionGhcOptions globalOptions ++ sectionGhcOptions options
  , sectionGhcProfOptions = sectionGhcProfOptions globalOptions ++ sectionGhcProfOptions options
  , sectionCppOptions = sectionCppOptions globalOptions ++ sectionCppOptions options
  , sectionCCOptions = sectionCCOptions globalOptions ++ sectionCCOptions options
  , sectionCSources = sectionCSources globalOptions ++ sectionCSources options
  , sectionExtraLibDirs = sectionExtraLibDirs globalOptions ++ sectionExtraLibDirs options
  , sectionExtraLibraries = sectionExtraLibraries globalOptions ++ sectionExtraLibraries options
  , sectionIncludeDirs = sectionIncludeDirs globalOptions ++ sectionIncludeDirs options
  , sectionInstallIncludes = sectionInstallIncludes globalOptions ++ sectionInstallIncludes options
  , sectionLdOptions = sectionLdOptions globalOptions ++ sectionLdOptions options
  , sectionBuildable = sectionBuildable options <|> sectionBuildable globalOptions
  , sectionDependencies = sectionDependencies globalOptions ++ sectionDependencies options
  , sectionConditionals = sectionConditionals globalOptions ++ sectionConditionals options
  , sectionBuildTools = sectionBuildTools globalOptions ++ sectionBuildTools options
  }

toSection :: a -> CommonOptions -> ([FieldName], Section a)
toSection a CommonOptions{..}
  = ( concat unknownFields
    , Section {
        sectionData = a
      , sectionSourceDirs = fromMaybeList commonOptionsSourceDirs
      , sectionDefaultExtensions = fromMaybeList commonOptionsDefaultExtensions
      , sectionOtherExtensions = fromMaybeList commonOptionsOtherExtensions
      , sectionGhcOptions = fromMaybeList commonOptionsGhcOptions
      , sectionGhcProfOptions = fromMaybeList commonOptionsGhcProfOptions
      , sectionCppOptions = fromMaybeList commonOptionsCppOptions
      , sectionCCOptions = fromMaybeList commonOptionsCCOptions
      , sectionCSources = fromMaybeList commonOptionsCSources
      , sectionExtraLibDirs = fromMaybeList commonOptionsExtraLibDirs
      , sectionExtraLibraries = fromMaybeList commonOptionsExtraLibraries
      , sectionIncludeDirs = fromMaybeList commonOptionsIncludeDirs
      , sectionInstallIncludes = fromMaybeList commonOptionsInstallIncludes
      , sectionLdOptions = fromMaybeList commonOptionsLdOptions
      , sectionBuildable = commonOptionsBuildable
      , sectionDependencies = fromMaybeList commonOptionsDependencies
      , sectionConditionals = conditionals
      , sectionBuildTools = fromMaybeList commonOptionsBuildTools
      }
    )
  where
    (unknownFields, conditionals) = unzip (map toConditional $ fromMaybeList commonOptionsWhen)

toConditional :: ConditionalSection -> ([FieldName], Conditional)
toConditional x = case x of
  ThenElseConditional (CaptureUnknownFields fields (ThenElse condition (CaptureUnknownFields fieldsThen then_) (CaptureUnknownFields fieldsElse else_))) ->
      (fields ++ fieldsThen ++ fieldsElse, Conditional condition (() <$ then_) (Just (() <$ else_)))
  FlatConditional (CaptureUnknownFields fields sect) -> (fields, Conditional (conditionCondition $ sectionData sect) (() <$ sect) Nothing)

pathsModuleFromPackageName :: String -> String
pathsModuleFromPackageName name = "Paths_" ++ map f name
  where
    f '-' = '_'
    f x = x

determineModules :: String -> [String] -> Maybe (List String) -> Maybe (List String) -> ([String], [String])
determineModules name modules mExposedModules mOtherModules = case (mExposedModules, mOtherModules) of
  (Nothing, Nothing) -> (modules, [])
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
      removeSetup src . toModules <$> getFilesRecursive src
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
