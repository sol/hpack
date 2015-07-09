{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module Hpack.Run (
  run
-- exported for testing
, renderPackage
, renderSourceRepository
, formatDescription
) where

import           Prelude ()
import           Prelude.Compat

import           Control.Monad
import           Data.Char
import           Data.Maybe
import           Data.List.Compat
import           System.Exit.Compat

import           Hpack.Util
import           Hpack.Config
import           Hpack.Render

run :: IO ([String], FilePath, String)
run = do
  mPackage <- readPackageConfig packageConfig
  case mPackage of
    Right (warnings, package) -> do
      let cabalFile = packageName package ++ ".cabal"

      old <- tryReadFile cabalFile

      let alignment = fromMaybe 16 (old >>= sniffAlignment)
          output = renderPackage alignment (maybe [] extractFieldOrderHint old) package
      return (warnings, cabalFile, output)
    Left err -> die err

renderPackage :: Int -> [String] -> Package -> String
renderPackage alignment existingFieldOrder Package{..} = intercalate "\n" chunks
  where
    chunks :: [String]
    chunks = catMaybes [
        header
      , extraSourceFiles
      , dataFiles
      , sourceRepository
      ] ++ map (unlines . render 0) section

    header = Just (unlines $ map formatField sortedFields)

    extraSourceFiles = guard (not . null $ packageExtraSourceFiles) >> Just (unlines $ "extra-source-files:" : map ("  " ++) packageExtraSourceFiles)
    dataFiles = guard (not . null $ packageDataFiles) >> Just (unlines $ "data-files:" : map ("  " ++) packageDataFiles)

    sourceRepository = renderSourceRepository <$> packageSourceRepository

    library = maybe [] (return . renderLibrary) packageLibrary

    section = library ++ renderExecutables packageExecutables ++ renderTests packageTests

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
      , ("build-type", Just "Simple")
      , ("cabal-version", Just ">= 1.10")
      ]

    formatList :: [String] -> Maybe String
    formatList xs = guard (not $ null xs) >> (Just $ intercalate separator xs)
      where
        separator = ",\n" ++ replicate alignment ' '

    defaultFieldOrder :: [String]
    defaultFieldOrder = map fst fields

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

renderSourceRepository :: SourceRepository -> String
renderSourceRepository SourceRepository{..} = concat [
    "source-repository head\n"
  , "  type: git\n"
  , "  location: " ++ sourceRepositoryUrl ++ "\n"
  , maybe "" (("  subdir: " ++) . (++ "\n")) sourceRepositorySubdir
  ]

renderExecutables :: [Section Executable] -> [Stanza]
renderExecutables = map renderExecutable

renderExecutable :: Section Executable -> Stanza
renderExecutable section@(sectionData -> Executable{..}) =
  Stanza ("executable " ++ executableName) (renderExecutableSection section)

renderTests :: [Section Executable] -> [Stanza]
renderTests = map renderTest

renderTest :: Section Executable -> Stanza
renderTest section@(sectionData -> Executable{..}) =
  Stanza ("test-suite " ++ executableName)
    (Field "type" "exitcode-stdio-1.0" : renderExecutableSection section)

renderExecutableSection :: Section Executable -> [Field]
renderExecutableSection section@(sectionData -> Executable{..}) =
  mainIs : renderSection section ++ [otherModules, defaultLanguage]
  where
    mainIs = Field "main-is" (Literal executableMain)
    otherModules = renderOtherModules executableOtherModules

renderLibrary :: Section Library -> Stanza
renderLibrary section@(sectionData -> Library{..}) = Stanza "library" $
  renderSection section ++ [
    renderExposedModules libraryExposedModules
  , renderOtherModules libraryOtherModules
  , defaultLanguage
  ]

renderSection :: Section a -> [Field]
renderSection Section{..} = [
    renderSourceDirs sectionSourceDirs
  , renderDependencies sectionDependencies
  , renderDefaultExtensions sectionDefaultExtensions
  , renderGhcOptions sectionGhcOptions
  , renderCppOptions sectionCppOptions
  ]

defaultLanguage :: Field
defaultLanguage = Field "default-language" "Haskell2010"

renderSourceDirs :: [String] -> Field
renderSourceDirs dirs = Field "hs-source-dirs" (CommaSeparatedList dirs)

renderExposedModules :: [String] -> Field
renderExposedModules modules = Field "exposed-modules" (LineSeparatedList modules)

renderOtherModules :: [String] -> Field
renderOtherModules modules = Field "other-modules" (LineSeparatedList modules)

renderDependencies :: [Dependency] -> Field
renderDependencies dependencies = Field "build-depends" (CommaSeparatedList $ map dependencyName dependencies)

renderGhcOptions :: [GhcOption] -> Field
renderGhcOptions = Field "ghc-options" . WordList

renderCppOptions :: [GhcOption] -> Field
renderCppOptions = Field "cpp-options" . WordList

renderDefaultExtensions :: [String] -> Field
renderDefaultExtensions = Field "default-extensions" . WordList
