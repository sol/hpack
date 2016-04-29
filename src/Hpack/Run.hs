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
, renderFlag
, renderSourceRepository
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

run :: FilePath -> FileName -> IO ([String], FilePath, String)
run dir file = do
  mPackage <- readPackageConfig (dir </> file)
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

    sourceRepository = maybe [] (return . renderSourceRepository) packageSourceRepository

    library = maybe [] (return . renderLibrary) packageLibrary

    stanzas :: [Element]
    stanzas =
      extraSourceFiles
      : dataFiles
      : sourceRepository
      ++ concat [
        map renderFlag packageFlags
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
      , ("license-file", packageLicenseFile)
      , ("tested-with", packageTestedWith)
      , ("build-type", Just "Simple")
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
      libCabalVersion :: Section Library -> Maybe String
      libCabalVersion sect = ">= 1.21" <$ guard (hasReexportedModules sect)

      hasReexportedModules :: Section Library -> Bool
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

renderExecutables :: [Section Executable] -> [Element]
renderExecutables = map renderExecutable

renderExecutable :: Section Executable -> Element
renderExecutable sect@(sectionData -> Executable{..}) =
  Stanza ("executable " ++ executableName) (renderExecutableSection sect)

renderTests :: [Section Executable] -> [Element]
renderTests = map renderTest

renderTest :: Section Executable -> Element
renderTest sect@(sectionData -> Executable{..}) =
  Stanza ("test-suite " ++ executableName)
    (Field "type" "exitcode-stdio-1.0" : renderExecutableSection sect)

renderBenchmarks :: [Section Executable] -> [Element]
renderBenchmarks = map renderBenchmark

renderBenchmark :: Section Executable -> Element
renderBenchmark sect@(sectionData -> Executable{..}) =
  Stanza ("benchmark " ++ executableName)
    (Field "type" "exitcode-stdio-1.0" : renderExecutableSection sect)

renderExecutableSection :: Section Executable -> [Element]
renderExecutableSection sect@(sectionData -> Executable{..}) =
  mainIs : renderSection sect ++ [otherModules, defaultLanguage]
  where
    mainIs = Field "main-is" (Literal executableMain)
    otherModules = renderOtherModules executableOtherModules

renderLibrary :: Section Library -> Element
renderLibrary sect@(sectionData -> Library{..}) = Stanza "library" $
  renderSection sect ++
  maybe [] (return . renderExposed) libraryExposed ++ [
    renderExposedModules libraryExposedModules
  , renderOtherModules libraryOtherModules
  , renderReexportedModules libraryReexportedModules
  , defaultLanguage
  ]

renderExposed :: Bool -> Element
renderExposed = Field "exposed" . Literal . show

renderSection :: Section a -> [Element]
renderSection Section{..} = [
    renderSourceDirs sectionSourceDirs
  , renderDefaultExtensions sectionDefaultExtensions
  , renderOtherExtensions sectionOtherExtensions
  , renderGhcOptions sectionGhcOptions
  , renderGhcProfOptions sectionGhcProfOptions
  , renderCppOptions sectionCppOptions
  , Field "include-dirs" (LineSeparatedList sectionIncludeDirs)
  , Field "install-includes" (LineSeparatedList sectionInstallIncludes)
  , Field "c-sources" (LineSeparatedList sectionCSources)
  , Field "extra-lib-dirs" (LineSeparatedList sectionExtraLibDirs)
  , Field "extra-libraries" (LineSeparatedList sectionExtraLibraries)
  , renderLdOptions sectionLdOptions
  , renderDependencies sectionDependencies
  ]
  ++ maybe [] (return . renderBuildable) sectionBuildable
  ++ map renderConditional sectionConditionals

renderConditional :: Conditional -> Element
renderConditional (Conditional condition sect mElse) = case mElse of
  Nothing -> if_
  Just else_ -> Group if_ (Stanza "else" $ renderSection else_)
  where
    if_ = Stanza ("if " ++ condition) (renderSection sect)

defaultLanguage :: Element
defaultLanguage = Field "default-language" "Haskell2010"

renderSourceDirs :: [String] -> Element
renderSourceDirs = Field "hs-source-dirs" . CommaSeparatedList

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

renderCppOptions :: [CppOption] -> Element
renderCppOptions = Field "cpp-options" . WordList

renderLdOptions :: [LdOption] -> Element
renderLdOptions = Field "ld-options" . WordList

renderBuildable :: Bool -> Element
renderBuildable = Field "buildable" . Literal . show

renderDefaultExtensions :: [String] -> Element
renderDefaultExtensions = Field "default-extensions" . WordList

renderOtherExtensions :: [String] -> Element
renderOtherExtensions = Field "other-extensions" . WordList
