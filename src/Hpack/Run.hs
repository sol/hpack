{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE CPP #-}
module Hpack.Run (
  run
, renderPackage
, RenderSettings(..)
, CommaStyle(..)
, defaultRenderSettings
#ifdef TEST
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

run :: FilePath -> IO ([String], FilePath, String)
run dir = do
  mPackage <- readPackageConfig (dir </> packageConfig)
  case mPackage of
    Right (warnings, pkg) -> do
      let cabalFile = dir </> (packageName pkg ++ ".cabal")

      old <- tryReadFile cabalFile

      let alignment = fromMaybe 16 (old >>= sniffAlignment)
          settings = maybe defaultRenderSettings sniffRenderSettings old
          output = renderPackage settings alignment (maybe [] extractFieldOrderHint old) pkg
      return (warnings, cabalFile, output)
    Left err -> die err

renderPackage :: RenderSettings -> Int -> [String] -> Package -> String
renderPackage settings alignment existingFieldOrder Package{..} = intercalate "\n" (header : chunks)
  where
    chunks :: [String]
    chunks = map unlines . filter (not . null) . map (render settings 0) $ stanzas

    header = unlines $ map formatField sortedFields

    extraSourceFiles :: Element
    extraSourceFiles = Field "extra-source-files" (LineSeparatedList packageExtraSourceFiles)

    dataFiles :: Element
    dataFiles = Field "data-files" (LineSeparatedList packageDataFiles)

    sourceRepository = maybe [] (return . renderSourceRepository) packageSourceRepository

    library = maybe [] (return . renderLibrary) packageLibrary

    stanzas :: [Element]
    stanzas = extraSourceFiles
            : dataFiles
            : sourceRepository
            ++ library
            ++ renderExecutables packageExecutables
            ++ renderTests packageTests
            ++ renderBenchmarks packageBenchmarks

    padding name = replicate (alignment - length name - 2) ' '

    formatField :: (String, String) -> String
    formatField (name, value) = name ++ ": " ++ padding name ++ value

    sortedFields :: [(String, String)]
    sortedFields = foldr insertByDefaultFieldOrder (sortBy orderingForExistingFields existing) new
      where
        (existing, new) = partition ((`elem` existingFieldOrder) . fst) fields

        insertByDefaultFieldOrder :: (String, a) -> [(String, a)] -> [(String, a)]
        insertByDefaultFieldOrder x@(key1, _) xs = case xs of
          [] -> [x]
          y@(key2, _) : ys -> if index key1 < index key2 then x : y : ys else y : insertByDefaultFieldOrder x ys
          where
            index :: String -> Maybe Int
            index = (`elemIndex` defaultFieldOrder)

    orderingForExistingFields :: (String, a) -> (String, a) -> Ordering
    orderingForExistingFields (key1, _) (key2, _) = index key1 `compare` index key2
      where
        index :: String -> Maybe Int
        index = (`elemIndex` existingFieldOrder)

    fields :: [(String, String)]
    fields = mapMaybe (\(name, value) -> (,) name <$> value) $ [
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
        separator = ",\n" ++ replicate alignment ' '

    defaultFieldOrder :: [String]
    defaultFieldOrder = map fst fields

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

formatDescription :: Int -> String -> String
formatDescription alignment description = case map emptyLineToDot $ lines description of
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
  renderSection sect ++ [
    renderExposedModules libraryExposedModules
  , renderOtherModules libraryOtherModules
  , renderReexportedModules libraryReexportedModules
  , defaultLanguage
  ]

renderSection :: Section a -> [Element]
renderSection Section{..} = [
    renderSourceDirs sectionSourceDirs
  , renderDefaultExtensions sectionDefaultExtensions
  , renderOtherExtensions sectionOtherExtensions
  , renderGhcOptions sectionGhcOptions
  , renderGhcProfOptions sectionGhcProfOptions
  , renderCppOptions sectionCppOptions
  , renderDependencies sectionDependencies
  ]

defaultLanguage :: Element
defaultLanguage = Field "default-language" "Haskell2010"

renderSourceDirs :: [String] -> Element
renderSourceDirs dirs = Field "hs-source-dirs" (CommaSeparatedList dirs)

renderExposedModules :: [String] -> Element
renderExposedModules modules = Field "exposed-modules" (LineSeparatedList modules)

renderOtherModules :: [String] -> Element
renderOtherModules modules = Field "other-modules" (LineSeparatedList modules)

renderReexportedModules :: [String] -> Element
renderReexportedModules modules = Field "reexported-modules" (LineSeparatedList modules)

renderDependencies :: [Dependency] -> Element
renderDependencies dependencies = Field "build-depends" (CommaSeparatedList $ map dependencyName dependencies)

renderGhcOptions :: [GhcOption] -> Element
renderGhcOptions = Field "ghc-options" . WordList

renderGhcProfOptions :: [GhcProfOption] -> Element
renderGhcProfOptions = Field "ghc-prof-options" . WordList

renderCppOptions :: [CppOption] -> Element
renderCppOptions = Field "cpp-options" . WordList

renderDefaultExtensions :: [String] -> Element
renderDefaultExtensions = Field "default-extensions" . WordList

renderOtherExtensions :: [String] -> Element
renderOtherExtensions = Field "other-extensions" . WordList
