{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE CPP #-}
module Hpack.Render (
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

  renderPackage
, renderPackageWith
, defaultRenderSettings
, RenderSettings(..)
, Alignment(..)
, CommaStyle(..)
#ifdef TEST
, renderConditional
, renderDependencies
, renderLibraryFields
, renderExecutableFields
, renderFlag
, renderSourceRepository
, renderDirectories
, formatDescription
#endif
) where

import           Imports

import           Data.Char
import           Data.Maybe
import           Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map

import           Hpack.Util
import           Hpack.Config
import           Hpack.Render.Hints
import           Hpack.Render.Dsl hiding (sortFieldsBy)
import qualified Hpack.Render.Dsl as Dsl

renderPackage :: [String] -> Package -> String
renderPackage oldCabalFile = renderPackageWith settings headerFieldsAlignment formattingHintsFieldOrder formattingHintsSectionsFieldOrder
  where
    FormattingHints{..} = sniffFormattingHints oldCabalFile
    headerFieldsAlignment = fromMaybe 16 formattingHintsAlignment
    settings = formattingHintsRenderSettings

renderPackageWith :: RenderSettings -> Alignment -> [String] -> [(String, [String])] -> Package -> String
renderPackageWith settings headerFieldsAlignment existingFieldOrder sectionsFieldOrder Package{..} = intercalate "\n" (unlines header : chunks)
  where
    chunks :: [String]
    chunks = map unlines . filter (not . null) . map (render settings 0) $ sortStanzaFields sectionsFieldOrder stanzas

    header :: [String]
    header = concatMap (render settings {renderSettingsFieldAlignment = headerFieldsAlignment} 0) packageFields

    packageFields :: [Element]
    packageFields = addVerbatim packageVerbatim . sortFieldsBy existingFieldOrder $
      headerFields ++ [
        Field "tested-with" $ CommaSeparatedList packageTestedWith
      , Field "extra-source-files" (renderPaths packageExtraSourceFiles)
      , Field "extra-doc-files" (renderPaths packageExtraDocFiles)
      , Field "data-files" (renderPaths packageDataFiles)
      ] ++ maybe [] (return . Field "data-dir" . Literal) packageDataDir

    sourceRepository :: [Element]
    sourceRepository = maybe [] (return . renderSourceRepository) packageSourceRepository

    customSetup :: [Element]
    customSetup = maybe [] (return . renderCustomSetup) packageCustomSetup

    library :: [Element]
    library = maybe [] (return . renderLibrary) packageLibrary

    stanzas :: [Element]
    stanzas = concat [
        sourceRepository
      , customSetup
      , map renderFlag packageFlags
      , library
      , renderInternalLibraries packageInternalLibraries
      , renderExecutables packageExecutables
      , renderTests packageTests
      , renderBenchmarks packageBenchmarks
      ]

    headerFields :: [Element]
    headerFields = mapMaybe (\(name, value) -> Field name . Literal <$> value) $ [
        ("name", Just packageName)
      , ("version", Just packageVersion)
      , ("synopsis", packageSynopsis)
      , ("description", (formatDescription headerFieldsAlignment <$> packageDescription))
      , ("category", packageCategory)
      , ("stability", packageStability)
      , ("homepage", packageHomepage)
      , ("bug-reports", packageBugReports)
      , ("author", formatList packageAuthor)
      , ("maintainer", formatList packageMaintainer)
      , ("copyright", formatList packageCopyright)
      , ("license", packageLicense)
      , case packageLicenseFile of
          [file] -> ("license-file", Just file)
          files  -> ("license-files", formatList files)
      , ("build-type", Just (show packageBuildType))
      ]

    formatList :: [String] -> Maybe String
    formatList xs = guard (not $ null xs) >> (Just $ intercalate separator xs)
      where
        separator = let Alignment n = headerFieldsAlignment in ",\n" ++ replicate n ' '

sortStanzaFields :: [(String, [String])] -> [Element] -> [Element]
sortStanzaFields sectionsFieldOrder = go
  where
    go sections = case sections of
      [] -> []
      Stanza name fields : xs | Just fieldOrder <- lookup name sectionsFieldOrder -> Stanza name (sortFieldsBy fieldOrder fields) : go xs
      x : xs -> x : go xs

formatDescription :: Alignment -> String -> String
formatDescription (Alignment alignment) description = case map emptyLineToDot $ lines description of
  x : xs -> intercalate "\n" (x : map (indentation ++) xs)
  [] -> ""
  where
    n = max alignment (length ("description: " :: String))
    indentation = replicate n ' '

    emptyLineToDot xs
      | isEmptyLine xs = "."
      | otherwise = xs

    isEmptyLine = all isSpace

renderSourceRepository :: SourceRepository -> Element
renderSourceRepository SourceRepository{..} = Stanza "source-repository head" [
    Field "type" "git"
  , Field "location" (Literal sourceRepositoryUrl)
  , Field "subdir" (maybe "" Literal sourceRepositorySubdir)
  ]

renderFlag :: Flag -> Element
renderFlag Flag {..} = Stanza ("flag " ++ flagName) $ description ++ [
    Field "manual" (Literal $ show flagManual)
  , Field "default" (Literal $ show flagDefault)
  ]
  where
    description = maybe [] (return . Field "description" . Literal) flagDescription

renderInternalLibraries :: Map String (Section Library) -> [Element]
renderInternalLibraries = map renderInternalLibrary . Map.toList

renderInternalLibrary :: (String, Section Library) -> Element
renderInternalLibrary (name, sect) =
  Stanza ("library " ++ name) (renderLibrarySection sect)

renderExecutables :: Map String (Section Executable) -> [Element]
renderExecutables = map renderExecutable . Map.toList

renderExecutable :: (String, Section Executable) -> Element
renderExecutable (name, sect) =
  Stanza ("executable " ++ name) (renderExecutableSection [] sect)

renderTests :: Map String (Section Executable) -> [Element]
renderTests = map renderTest . Map.toList

renderTest :: (String, Section Executable) -> Element
renderTest (name, sect) =
  Stanza ("test-suite " ++ name)
    (renderExecutableSection [Field "type" "exitcode-stdio-1.0"] sect)

renderBenchmarks :: Map String (Section Executable) -> [Element]
renderBenchmarks = map renderBenchmark . Map.toList

renderBenchmark :: (String, Section Executable) -> Element
renderBenchmark (name, sect) =
  Stanza ("benchmark " ++ name)
    (renderExecutableSection [Field "type" "exitcode-stdio-1.0"] sect)

renderExecutableSection :: [Element] -> Section Executable -> [Element]
renderExecutableSection extraFields = renderSection renderExecutableFields extraFields

renderExecutableFields :: Executable -> [Element]
renderExecutableFields Executable{..} = mainIs ++ [otherModules, generatedModules]
  where
    mainIs = maybe [] (return . Field "main-is" . Literal) executableMain
    otherModules = renderOtherModules executableOtherModules
    generatedModules = renderGeneratedModules executableGeneratedModules

renderCustomSetup :: CustomSetup -> Element
renderCustomSetup CustomSetup{..} =
  Stanza "custom-setup" $ renderDependencies "setup-depends" customSetupDependencies

renderLibrary :: Section Library -> Element
renderLibrary sect = Stanza "library" $ renderLibrarySection sect

renderLibrarySection :: Section Library -> [Element]
renderLibrarySection = renderSection renderLibraryFields []

renderLibraryFields :: Library -> [Element]
renderLibraryFields Library{..} =
  maybe [] (return . renderExposed) libraryExposed ++
  maybe [] (return . renderVisibility) libraryVisibility ++ [
    renderExposedModules libraryExposedModules
  , renderOtherModules libraryOtherModules
  , renderGeneratedModules libraryGeneratedModules
  , renderReexportedModules libraryReexportedModules
  , renderSignatures librarySignatures
  ]

renderExposed :: Bool -> Element
renderExposed = Field "exposed" . Literal . show

renderVisibility :: String -> Element
renderVisibility = Field "visibility" . Literal

renderSection :: (a -> [Element]) -> [Element] -> Section a -> [Element]
renderSection renderSectionData extraFieldsStart Section{..} = addVerbatim sectionVerbatim $
     extraFieldsStart
  ++ renderSectionData sectionData ++ [
    renderDirectories "hs-source-dirs" sectionSourceDirs
  , renderDefaultExtensions sectionDefaultExtensions
  , renderOtherExtensions sectionOtherExtensions
  , renderGhcOptions sectionGhcOptions
  , renderGhcProfOptions sectionGhcProfOptions
  , renderGhcSharedOptions sectionGhcSharedOptions
  , renderGhcjsOptions sectionGhcjsOptions
  , renderCppOptions sectionCppOptions
  , renderCcOptions sectionCcOptions
  , renderCxxOptions sectionCxxOptions
  , renderDirectories "include-dirs" sectionIncludeDirs
  , Field "install-includes" (LineSeparatedList sectionInstallIncludes)
  , Field "c-sources" (renderPaths sectionCSources)
  , Field "cxx-sources" (renderPaths sectionCxxSources)
  , Field "js-sources" (renderPaths sectionJsSources)
  , renderDirectories "extra-lib-dirs" sectionExtraLibDirs
  , Field "extra-libraries" (LineSeparatedList sectionExtraLibraries)
  , renderDirectories "extra-frameworks-dirs" sectionExtraFrameworksDirs
  , Field "frameworks" (LineSeparatedList sectionFrameworks)
  , renderLdOptions sectionLdOptions
  , Field "pkgconfig-depends" (CommaSeparatedList sectionPkgConfigDependencies)
  ]
  ++ renderBuildTools sectionBuildTools sectionSystemBuildTools
  ++ renderDependencies "build-depends" sectionDependencies
  ++ maybe [] (return . renderBuildable) sectionBuildable
  ++ maybe [] (return . renderLanguage) sectionLanguage
  ++ map (renderConditional renderSectionData) sectionConditionals

addVerbatim :: [Verbatim] -> [Element] -> [Element]
addVerbatim verbatim fields = filterVerbatim verbatim fields ++ renderVerbatim verbatim

filterVerbatim :: [Verbatim] -> [Element] -> [Element]
filterVerbatim verbatim = filter p
  where
    p :: Element -> Bool
    p = \ case
      Field name _ -> name `notElem` fields
      _ -> True
    fields = concatMap verbatimFieldNames verbatim

verbatimFieldNames :: Verbatim -> [String]
verbatimFieldNames verbatim = case verbatim of
  VerbatimLiteral _ -> []
  VerbatimObject o -> Map.keys o

renderVerbatim :: [Verbatim] -> [Element]
renderVerbatim = concatMap $ \ case
  VerbatimLiteral s -> [Verbatim s]
  VerbatimObject o -> renderVerbatimObject o

renderVerbatimObject :: Map String VerbatimValue -> [Element]
renderVerbatimObject = map renderPair . Map.toList
  where
    renderPair (key, value) = case lines (verbatimValueToString value) of
      [x] -> Field key (Literal x)
      xs -> Field key (LineSeparatedList xs)

renderConditional :: (a -> [Element]) -> Conditional (Section a) -> Element
renderConditional renderSectionData (Conditional condition sect mElse) = case mElse of
  Nothing -> if_
  Just else_ -> Group if_ (Stanza "else" $ renderSection renderSectionData [] else_)
  where
    if_ = Stanza ("if " ++ renderCond condition) (renderSection renderSectionData [] sect)

renderCond :: Cond -> String
renderCond = \ case
  CondExpression c -> c
  CondBool True -> "true"
  CondBool False -> "false"

renderDirectories :: String -> [String] -> Element
renderDirectories name = Field name . LineSeparatedList . replaceDots
  where
    replaceDots = map replaceDot
    replaceDot xs = case xs of
      "." -> "./"
      _ -> xs

renderExposedModules :: [Module] -> Element
renderExposedModules = Field "exposed-modules" . LineSeparatedList . map unModule

renderOtherModules :: [Module] -> Element
renderOtherModules = Field "other-modules" . LineSeparatedList . map unModule

renderGeneratedModules :: [Module] -> Element
renderGeneratedModules = Field "autogen-modules" . LineSeparatedList . map unModule

renderReexportedModules :: [String] -> Element
renderReexportedModules = Field "reexported-modules" . CommaSeparatedList

renderSignatures :: [String] -> Element
renderSignatures = Field "signatures" . CommaSeparatedList

renderDependencies :: String -> Dependencies -> [Element]
renderDependencies name deps = [
    Field name (CommaSeparatedList renderedDeps)
  , Field "mixins" (CommaSeparatedList $ concat mixins)
  ]
  where
    (renderedDeps, mixins) = unzip . map renderDependency . Map.toList $ unDependencies deps

renderDependency :: (String, DependencyInfo) -> (String, [String])
renderDependency (name, DependencyInfo mixins version) = (
      name ++ renderVersion version
    , [ name ++ " " ++ mixin | mixin <- mixins ]
    )

renderVersion :: DependencyVersion -> String
renderVersion (DependencyVersion _ c) = renderVersionConstraint c

renderVersionConstraint :: VersionConstraint -> String
renderVersionConstraint version = case version of
  AnyVersion -> ""
  VersionRange x -> " " ++ x

renderBuildTools :: Map BuildTool DependencyVersion -> SystemBuildTools -> [Element]
renderBuildTools (map renderBuildTool . Map.toList -> xs) systemBuildTools = [
    Field "build-tools" (CommaSeparatedList $ [x | BuildTools x <- xs] ++ renderSystemBuildTools systemBuildTools)
  , Field "build-tool-depends" (CommaSeparatedList [x | BuildToolDepends x <- xs])
  ]

data RenderBuildTool = BuildTools String | BuildToolDepends String

renderBuildTool :: (BuildTool,  DependencyVersion) -> RenderBuildTool
renderBuildTool (buildTool, renderVersion -> version) = case buildTool of
  LocalBuildTool executable -> BuildTools (executable ++ version)
  BuildTool pkg executable
    | pkg == executable && executable `elem` knownBuildTools -> BuildTools (executable ++ version)
    | otherwise -> BuildToolDepends (pkg ++ ":" ++ executable ++ version)
  where
    knownBuildTools :: [String]
    knownBuildTools = [
        "alex"
      , "c2hs"
      , "cpphs"
      , "greencard"
      , "haddock"
      , "happy"
      , "hsc2hs"
      , "hscolour"
      ]

renderSystemBuildTools :: SystemBuildTools -> [String]
renderSystemBuildTools = map renderSystemBuildTool . Map.toList . unSystemBuildTools

renderSystemBuildTool :: (String, VersionConstraint) -> String
renderSystemBuildTool (name, constraint) = name ++ renderVersionConstraint constraint

renderLanguage :: Language -> Element
renderLanguage (Language lang) = Field "default-language" (Literal lang)

renderGhcOptions :: [GhcOption] -> Element
renderGhcOptions = Field "ghc-options" . WordList

renderGhcProfOptions :: [GhcProfOption] -> Element
renderGhcProfOptions = Field "ghc-prof-options" . WordList

renderGhcSharedOptions :: [GhcOption] -> Element
renderGhcSharedOptions = Field "ghc-shared-options" . WordList

renderGhcjsOptions :: [GhcjsOption] -> Element
renderGhcjsOptions = Field "ghcjs-options" . WordList

renderCppOptions :: [CppOption] -> Element
renderCppOptions = Field "cpp-options" . WordList

renderCcOptions :: [CcOption] -> Element
renderCcOptions = Field "cc-options" . WordList

renderCxxOptions :: [CxxOption] -> Element
renderCxxOptions = Field "cxx-options" . WordList

renderLdOptions :: [LdOption] -> Element
renderLdOptions = Field "ld-options" . WordList

renderBuildable :: Bool -> Element
renderBuildable = Field "buildable" . Literal . show

renderDefaultExtensions :: [String] -> Element
renderDefaultExtensions = Field "default-extensions" . LineSeparatedList

renderOtherExtensions :: [String] -> Element
renderOtherExtensions = Field "other-extensions" . LineSeparatedList

renderPaths :: [Path] -> Value
renderPaths = LineSeparatedList . map renderPath
  where
    renderPath :: Path -> FilePath
    renderPath (Path path)
      | needsQuoting path = show path
      | otherwise = path

    needsQuoting :: FilePath -> Bool
    needsQuoting = any (\x -> isSpace x || x == ',')

sortFieldsBy :: [String] -> [Element] -> [Element]
sortFieldsBy existingFieldOrder = Dsl.sortFieldsBy ("import" : existingFieldOrder)
