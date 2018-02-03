{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE CPP #-}
module Hpack.Run (
  RunOptions(..)
, defaultRunOptions
, run
, renderPackage
, RenderSettings(..)
, Alignment(..)
, CommaStyle(..)
, defaultRenderSettings
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
import           System.Exit
import           System.FilePath
import           System.Directory
import           Data.Version
import           Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import qualified Data.Aeson as Aeson

import           Hpack.Util
import           Hpack.Config
import           Hpack.Dependency (scientificToVersion)
import           Hpack.Render
import           Hpack.FormattingHints
import           Hpack.Yaml

data RunOptions = RunOptions {
  runOptionsConfigDir :: Maybe FilePath
, runOptionsConfigFile :: FilePath
, runOptionsDecode :: FilePath -> IO (Either String Aeson.Value)
}

defaultRunOptions :: RunOptions
defaultRunOptions = RunOptions Nothing packageConfig decodeYaml

run :: RunOptions -> IO ([String], FilePath, String)
run (RunOptions mDir c decode) = do
  let dir = fromMaybe "" mDir
  userDataDir <- getAppUserDataDirectory "hpack"
  mPackage <- readPackageConfigWith decode userDataDir (dir </> c)
  case mPackage of
    Right (pkg, warnings) -> do
      let cabalFile = dir </> (packageName pkg ++ ".cabal")

      old <- tryReadFile cabalFile

      let
        FormattingHints{..} = sniffFormattingHints (fromMaybe "" old)
        alignment = fromMaybe 16 formattingHintsAlignment
        settings = formattingHintsRenderSettings

        output = renderPackage settings alignment formattingHintsFieldOrder formattingHintsSectionsFieldOrder pkg

      return (warnings, cabalFile, output)
    Left err -> die err

renderPackage :: RenderSettings -> Alignment -> [String] -> [(String, [String])] -> Package -> String
renderPackage settings alignment existingFieldOrder sectionsFieldOrder Package{..} = intercalate "\n" (unlines header : chunks)
  where
    chunks :: [String]
    chunks = map unlines . filter (not . null) . map (render settings 0) $ sortSectionFields sectionsFieldOrder stanzas

    header :: [String]
    header = concatMap (render settings {renderSettingsFieldAlignment = alignment} 0) (filterVerbatim packageVerbatim $ fields)

    extraSourceFiles :: Element
    extraSourceFiles = Field "extra-source-files" (LineSeparatedList packageExtraSourceFiles)

    extraDocFiles :: Element
    extraDocFiles = Field "extra-doc-files" (LineSeparatedList packageExtraDocFiles)

    dataFiles :: Element
    dataFiles = Field "data-files" (LineSeparatedList packageDataFiles)

    sourceRepository :: [Element]
    sourceRepository = maybe [] (return . renderSourceRepository) packageSourceRepository

    customSetup :: [Element]
    customSetup = maybe [] (return . renderCustomSetup) packageCustomSetup

    library :: [Element]
    library = maybe [] (return . renderLibrary) packageLibrary

    stanzas :: [Element]
    stanzas = addVerbatim packageVerbatim $
      extraSourceFiles
      : extraDocFiles
      : dataFiles
      : sourceRepository
      ++ concat [
        customSetup
      , map renderFlag packageFlags
      , library
      , renderInternalLibraries packageInternalLibraries
      , renderExecutables packageExecutables
      , renderTests packageTests
      , renderBenchmarks packageBenchmarks
      ]

    fields :: [Element]
    fields = sortFieldsBy existingFieldOrder . mapMaybe (\(name, value) -> Field name . Literal <$> value) $ [
        ("name", Just packageName)
      , ("version", Just packageVersion)
      , ("synopsis", packageSynopsis)
      , ("description", (formatDescription alignment <$> packageDescription))
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
      , ("cabal-version", cabalVersion)
      ]

    formatList :: [String] -> Maybe String
    formatList xs = guard (not $ null xs) >> (Just $ intercalate separator xs)
      where
        separator = let Alignment n = alignment in ",\n" ++ replicate n ' '

    cabalVersion :: Maybe String
    cabalVersion = (">= " ++) . showVersion <$> maximum [
        Just (makeVersion [1,10])
      , packageCabalVersion
      , packageLibrary >>= libraryCabalVersion
      , internalLibsCabalVersion packageInternalLibraries
      , executablesCabalVersion packageExecutables
      , executablesCabalVersion packageTests
      , executablesCabalVersion packageBenchmarks
      ]
     where
      packageCabalVersion :: Maybe Version
      packageCabalVersion = maximum [
          Nothing
        , makeVersion [1,24] <$ packageCustomSetup
        , makeVersion [1,18] <$ guard (not (null packageExtraDocFiles))
        ]

      libraryCabalVersion :: Section Library -> Maybe Version
      libraryCabalVersion sect = maximum [
          makeVersion [1,22] <$ guard hasReexportedModules
        , makeVersion [2,0]  <$ guard hasSignatures
        , makeVersion [2,0] <$ guard hasGeneratedModules
        ]
        where
          hasReexportedModules = any (not . null . libraryReexportedModules) sect
          hasSignatures = any (not . null . librarySignatures) sect
          hasGeneratedModules = any (not . null . libraryGeneratedModules) sect

      internalLibsCabalVersion :: Map String (Section Library) -> Maybe Version
      internalLibsCabalVersion internalLibraries = makeVersion [2,0] <$ guard (not (Map.null internalLibraries))

      executablesCabalVersion :: Map String (Section Executable) -> Maybe Version
      executablesCabalVersion = foldr max Nothing . map executableCabalVersion . Map.elems

      executableCabalVersion :: Section Executable -> Maybe Version
      executableCabalVersion sect = makeVersion [2,0] <$ guard (executableHasGeneratedModules sect)

      executableHasGeneratedModules :: Section Executable -> Bool
      executableHasGeneratedModules = any (not . null . executableGeneratedModules)

sortSectionFields :: [(String, [String])] -> [Element] -> [Element]
sortSectionFields sectionsFieldOrder = go
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
  , renderDirectories "include-dirs" sectionIncludeDirs
  , Field "install-includes" (LineSeparatedList sectionInstallIncludes)
  , Field "c-sources" (LineSeparatedList sectionCSources)
  , Field "js-sources" (LineSeparatedList sectionJsSources)
  , renderDirectories "extra-lib-dirs" sectionExtraLibDirs
  , Field "extra-libraries" (LineSeparatedList sectionExtraLibraries)
  , renderDirectories "extra-frameworks-dirs" sectionExtraFrameworksDirs
  , Field "frameworks" (LineSeparatedList sectionFrameworks)
  , renderLdOptions sectionLdOptions
  , renderDependencies "build-depends" sectionDependencies
  , Field "pkgconfig-depends" (CommaSeparatedList sectionPkgConfigDependencies)
  , renderDependencies "build-tools" sectionBuildTools
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
    renderPair (key, value) = case value of
      VerbatimString s -> case lines s of
        [x] -> Field key (Literal x)
        xs -> Field key (LineSeparatedList xs)
      VerbatimNumber n -> Field key (Literal $ scientificToVersion n)
      VerbatimBool b -> Field key (Literal $ show b)
      VerbatimNull -> Field key (Literal "")

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

renderDependency :: (String, DependencyVersion) -> String
renderDependency (name, version) = name ++ v
  where
    v = case version of
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

renderLdOptions :: [LdOption] -> Element
renderLdOptions = Field "ld-options" . WordList

renderBuildable :: Bool -> Element
renderBuildable = Field "buildable" . Literal . show

renderDefaultExtensions :: [String] -> Element
renderDefaultExtensions = Field "default-extensions" . WordList

renderOtherExtensions :: [String] -> Element
renderOtherExtensions = Field "other-extensions" . WordList
