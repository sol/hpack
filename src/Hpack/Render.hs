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
, renderLibraryFields
, renderExecutableFields
, renderFlag
, renderSourceRepository
, renderDirectories
, formatDescription
#endif
) where

import           Control.Monad
import           Data.Char
import           Data.Maybe
import           Data.List
import           Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map

import           Hpack.Util
import           Hpack.Config
import           Hpack.Render.Hints
import           Hpack.Render.Dsl

renderPackage :: [String] -> Package -> String
renderPackage oldCabalFile = renderPackageWith settings alignment formattingHintsFieldOrder formattingHintsSectionsFieldOrder
  where
    FormattingHints{..} = sniffFormattingHints oldCabalFile
    alignment = fromMaybe 16 formattingHintsAlignment
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
        Field "extra-source-files" (LineSeparatedList packageExtraSourceFiles)
      , Field "extra-doc-files" (LineSeparatedList packageExtraDocFiles)
      , Field "data-files" (LineSeparatedList packageDataFiles)
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
      , ("tested-with", packageTestedWith)
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
renderExecutable (name, sect@(sectionData -> Executable{..})) =
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
renderExecutableSection extraFields = renderSection renderExecutableFields extraFields [defaultLanguage]

renderExecutableFields :: Executable -> [Element]
renderExecutableFields Executable{..} = mainIs ++ [otherModules, generatedModules]
  where
    mainIs = maybe [] (return . Field "main-is" . Literal) executableMain
    otherModules = renderOtherModules executableOtherModules
    generatedModules = renderGeneratedModules executableGeneratedModules

renderCustomSetup :: CustomSetup -> Element
renderCustomSetup CustomSetup{..} =
  Stanza "custom-setup" [renderDependencies "setup-depends" customSetupDependencies]

renderLibrary :: Section Library -> Element
renderLibrary sect = Stanza "library" $ renderLibrarySection sect

renderLibrarySection :: Section Library -> [Element]
renderLibrarySection = renderSection renderLibraryFields [] [defaultLanguage]

renderLibraryFields :: Library -> [Element]
renderLibraryFields Library{..} =
  maybe [] (return . renderExposed) libraryExposed ++ [
    renderExposedModules libraryExposedModules
  , renderOtherModules libraryOtherModules
  , renderGeneratedModules libraryGeneratedModules
  , renderReexportedModules libraryReexportedModules
  , renderSignatures librarySignatures
  ]

renderExposed :: Bool -> Element
renderExposed = Field "exposed" . Literal . show

renderSection :: (a -> [Element]) -> [Element] -> [Element] -> Section a -> [Element]
renderSection renderSectionData extraFieldsStart extraFieldsEnd Section{..} = addVerbatim sectionVerbatim $
     extraFieldsStart
  ++ renderSectionData sectionData ++ [
    renderDirectories "hs-source-dirs" sectionSourceDirs
  , renderDefaultExtensions sectionDefaultExtensions
  , renderOtherExtensions sectionOtherExtensions
  , renderGhcOptions sectionGhcOptions
  , renderGhcProfOptions sectionGhcProfOptions
  , renderGhcjsOptions sectionGhcjsOptions
  , renderCppOptions sectionCppOptions
  , renderCcOptions sectionCcOptions
  , renderCxxOptions sectionCxxOptions
  , renderDirectories "include-dirs" sectionIncludeDirs
  , Field "install-includes" (LineSeparatedList sectionInstallIncludes)
  , Field "c-sources" (LineSeparatedList sectionCSources)
  , Field "cxx-sources" (LineSeparatedList sectionCxxSources)
  , Field "js-sources" (LineSeparatedList sectionJsSources)
  , renderDirectories "extra-lib-dirs" sectionExtraLibDirs
  , Field "extra-libraries" (LineSeparatedList sectionExtraLibraries)
  , renderDirectories "extra-frameworks-dirs" sectionExtraFrameworksDirs
  , Field "frameworks" (LineSeparatedList sectionFrameworks)
  , renderLdOptions sectionLdOptions
  , renderDependencies "build-depends" sectionDependencies
  , Field "pkgconfig-depends" (CommaSeparatedList sectionPkgConfigDependencies)
  , renderDependencies "build-tools" sectionBuildTools
  , renderBuildToolDepends sectionBuildToolDepends
  ]
  ++ maybe [] (return . renderBuildable) sectionBuildable
  ++ map (renderConditional renderSectionData) sectionConditionals
  ++ extraFieldsEnd

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
  Just else_ -> Group if_ (Stanza "else" $ renderSection renderSectionData [] [] else_)
  where
    if_ = Stanza ("if " ++ condition) (renderSection renderSectionData [] [] sect)

defaultLanguage :: Element
defaultLanguage = Field "default-language" "Haskell2010"

renderDirectories :: String -> [String] -> Element
renderDirectories name = Field name . LineSeparatedList . replaceDots
  where
    replaceDots = map replaceDot
    replaceDot xs = case xs of
      "." -> "./."
      _ -> xs

renderExposedModules :: [String] -> Element
renderExposedModules = Field "exposed-modules" . LineSeparatedList

renderOtherModules :: [String] -> Element
renderOtherModules = Field "other-modules" . LineSeparatedList

renderGeneratedModules :: [String] -> Element
renderGeneratedModules = Field "autogen-modules" . LineSeparatedList

renderReexportedModules :: [String] -> Element
renderReexportedModules = Field "reexported-modules" . LineSeparatedList

renderSignatures :: [String] -> Element
renderSignatures = Field "signatures" . CommaSeparatedList

renderDependencies :: String -> Dependencies -> Element
renderDependencies name = Field name . CommaSeparatedList . map renderDependency . Map.toList . unDependencies

renderBuildToolDepends :: BuildTools -> Element
renderBuildToolDepends = Field "build-tool-depends" . CommaSeparatedList . map renderBuildTool . Map.toList

renderBuildTool :: (BuildTool,  DependencyVersion) -> String
renderBuildTool (BuildTool pkg executable, version) = pkg ++ ":" ++ executable ++ renderVersion version

renderDependency :: (String, DependencyVersion) -> String
renderDependency (name, version) = name ++ renderVersion version

renderVersion :: DependencyVersion -> String
renderVersion version = case version of
  AnyVersion -> ""
  VersionRange x -> " " ++ x
  SourceDependency _ -> ""

renderGhcOptions :: [GhcOption] -> Element
renderGhcOptions = Field "ghc-options" . WordList

renderGhcProfOptions :: [GhcProfOption] -> Element
renderGhcProfOptions = Field "ghc-prof-options" . WordList

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
renderDefaultExtensions = Field "default-extensions" . WordList

renderOtherExtensions :: [String] -> Element
renderOtherExtensions = Field "other-extensions" . WordList
