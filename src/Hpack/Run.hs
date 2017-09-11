{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE CPP #-}
module Hpack.Run (
  run
, renderPackage
, RenderSettings(..)
, Alignment(..)
, CommaStyle(..)
, defaultRenderSettings
#ifdef TEST
, renderConditional
, renderExecutableSectionBody
, renderFlag
, renderLibraryBody
, renderSection
, renderSourceRepository
, renderDirectories
, formatDescription
#endif
) where

import           Prelude ()
import           Prelude.Compat

import           Control.Monad
import           Data.Char
import           Data.Maybe
import           Data.List.Compat
import           System.Exit.Compat
import           System.FilePath

import           Hpack.Util
import           Hpack.Config
import           Hpack.Render
import           Hpack.FormattingHints

run :: Maybe FilePath -> FilePath -> IO ([String], FilePath, String)
run mDir c = do
  let dir = fromMaybe "" mDir
  mPackage <- readPackageConfig (dir </> c)
  case mPackage of
    Right (warnings, pkg) -> do
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
    header = concatMap (render settings {renderSettingsFieldAlignment = alignment} 0) fields

    extraSourceFiles :: Element
    extraSourceFiles = Field "extra-source-files" (LineSeparatedList packageExtraSourceFiles)

    dataFiles :: Element
    dataFiles = Field "data-files" (LineSeparatedList packageDataFiles)

    sourceRepository :: [Element]
    sourceRepository = maybe [] (return . renderSourceRepository) packageSourceRepository

    customSetup :: [Element]
    customSetup = maybe [] (return . renderCustomSetup) packageCustomSetup

    library :: [Element]
    library = maybe [] (return . renderLibrary) packageLibrary

    stanzas :: [Element]
    stanzas =
      extraSourceFiles
      : dataFiles
      : sourceRepository
      ++ concat [
        customSetup
      , map renderFlag packageFlags
      , library
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
    cabalVersion = maximum [
        Just ">= 1.10"
      , packageLibrary >>= libCabalVersion
      ]
     where
      libCabalVersion :: Section Library Library -> Maybe String
      libCabalVersion sect = ">= 1.21" <$ guard (hasReexportedModules sect)

      hasReexportedModules :: Section Library Library -> Bool
      hasReexportedModules = not . null . libraryReexportedModules . sectionData

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

renderExecutables :: [Section Executable Executable] -> [Element]
renderExecutables = map renderExecutable

renderExecutable :: Section Executable Executable -> Element
renderExecutable sect@(sectionData -> Executable{..}) =
  Stanza ("executable " ++ fromMaybe "" executableName) (renderExecutableSection sect)

renderTests :: [Section Executable Executable] -> [Element]
renderTests = map renderTest

renderTest :: Section Executable Executable -> Element
renderTest sect@(sectionData -> Executable{..}) =
  Stanza ("test-suite " ++ fromMaybe "" executableName)
    (Field "type" "exitcode-stdio-1.0" : renderExecutableSection sect)

renderBenchmarks :: [Section Executable Executable] -> [Element]
renderBenchmarks = map renderBenchmark

renderBenchmark :: Section Executable Executable -> Element
renderBenchmark sect@(sectionData -> Executable{..}) =
  Stanza ("benchmark " ++ fromMaybe "" executableName)
    (Field "type" "exitcode-stdio-1.0" : renderExecutableSection sect)

renderExecutableSection :: Section Executable Executable -> [Element]
renderExecutableSection sect = renderExecutableSectionBody sect ++ [defaultLanguage]

renderExecutableSectionBody :: Section Executable Executable -> [Element]
renderExecutableSectionBody sect@(sectionData -> Executable{..}) =
  mainIs ++ renderSection renderExecutableSectionBody sect ++ [otherModules]
  where
    mainIs = maybe [] (pure . Field "main-is" . Literal) executableMain
    otherModules = renderOtherModules executableOtherModules

renderCustomSetup :: CustomSetup -> Element
renderCustomSetup CustomSetup{..} =
  Stanza "custom-setup" [renderSetupDepends customSetupDependencies]

renderLibrary :: Section Library Library -> Element
renderLibrary sect = Stanza "library" $ renderLibraryBody sect ++ [defaultLanguage]

renderLibraryBody :: Section Library Library -> [Element]
renderLibraryBody sect@(sectionData -> Library{..}) =
  renderSection renderLibraryBody sect ++
  maybe [] (return . renderExposed) libraryExposed ++ [
    renderExposedModules libraryExposedModules
  , renderOtherModules libraryOtherModules
  , renderReexportedModules libraryReexportedModules
  ]

renderExposed :: Bool -> Element
renderExposed = Field "exposed" . Literal . show

renderSection :: (Section b b -> [Element]) -> Section b a -> [Element]
renderSection renderAll Section{..} = [
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
  , renderLdOptions sectionLdOptions
  , renderDependencies sectionDependencies
  , renderBuildTools sectionBuildTools
  ]
  ++ maybe [] (return . renderBuildable) sectionBuildable
  ++ map (renderConditional renderAll) sectionConditionals

renderConditional :: (Section a a -> [Element]) -> Conditional a -> Element
renderConditional renderBody (Conditional condition sect mElse) = case mElse of
  Nothing -> if_
  Just else_ -> Group if_ (Stanza "else" $ renderBody else_)
  where
    if_ = Stanza ("if " ++ condition) (renderBody sect)

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

renderReexportedModules :: [String] -> Element
renderReexportedModules = Field "reexported-modules" . LineSeparatedList

renderDependencies :: [Dependency] -> Element
renderDependencies = Field "build-depends" . CommaSeparatedList . map dependencyName

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

renderBuildTools :: [Dependency] -> Element
renderBuildTools = Field "build-tools" . CommaSeparatedList . map dependencyName

renderSetupDepends :: [Dependency] -> Element
renderSetupDepends = Field "setup-depends" . CommaSeparatedList . map dependencyName
