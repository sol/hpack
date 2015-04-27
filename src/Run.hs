{-# LANGUAGE QuasiQuotes, RecordWildCards #-}
module Run (
  run
-- exported for testing
, renderPackage
) where

import           Control.Applicative
import           Control.Monad
import           Data.Maybe
import           Data.List
import           System.Exit.Compat
import           Data.Version (showVersion)

import           Paths_hpack (version)
import           Util
import           Config

configFile :: FilePath
configFile = "package.yaml"

run :: IO (FilePath, String)
run = do
  mPackage <- readPackageConfig configFile
  case mPackage of
    Right package -> do
      let cabalFile = packageName package ++ ".cabal"

      old <- tryReadFile cabalFile

      let alignment = fromMaybe 16 (old >>= sniffAlignment)
          output = concat [
              "-- This file has been generated from " ++ configFile ++ " by hpack version " ++ showVersion version ++ ".\n"
            , "--\n"
            , "-- see: https://github.com/sol/hpack\n"
            , "\n"
            , renderPackage alignment (maybe [] extractFieldOrderHint old) package
            ]
      return (cabalFile, output)
    Left err -> die err

renderPackage :: Int -> [String] -> Package -> String
renderPackage alignment existingFieldOrder Package{..} = intercalate "\n" sections
  where
    sections :: [String]
    sections = catMaybes [
        header
      , extraSourceFiles
      , sourceRepository
      , library
      ] ++ renderExecutables packageExecutables ++ renderTests packageTests

    header = Just (unlines $ map formatField sortedFields)

    extraSourceFiles = guard (not . null $ packageExtraSourceFiles) >> Just (unlines $ "extra-source-files:" : map ("  " ++) packageExtraSourceFiles)

    sourceRepository = renderSourceRepository <$> packageSourceRepository

    library = renderLibrary <$> packageLibrary

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
      , ("description", (formatDescription <$> packageDescription))
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

    formatDescription = intercalate separator . intersperse "." . lines
      where
        n = max alignment $ length ("description: ")
        separator = "\n" ++ replicate n ' '

    renderSourceRepository :: String -> String
    renderSourceRepository url = "source-repository head\n  type: git\n  location: " ++ url ++ "\n"

renderExecutables :: [Executable] -> [String]
renderExecutables = map renderExecutable

renderExecutable :: Executable -> String
renderExecutable executable@Executable{..} =
     "executable "
  ++ executableName ++ "\n"
  ++ renderExecutableSection executable

renderTests :: [Executable] -> [String]
renderTests = map renderTest

renderTest :: Executable -> String
renderTest executable@Executable{..} =
     "test-suite " ++ executableName ++ "\n"
  ++ "  type: exitcode-stdio-1.0\n"
  ++ renderExecutableSection executable

renderExecutableSection :: Executable -> String
renderExecutableSection Executable{..} = 
     renderSourceDirs executableSourceDirs
  ++ "  main-is: " ++ executableMain ++ "\n"
  ++ renderOtherModules executableOtherModules
  ++ renderDependencies executableDependencies 
  ++ renderDefaultExtensions executableDefaultExtensions
  ++ renderGhcOptions executableGhcOptions
  ++ "  default-language: Haskell2010\n"

renderLibrary :: Library -> String
renderLibrary Library{..} =
    "library\n"
  ++ renderSourceDirs librarySourceDirs
  ++ renderExposedModules libraryExposedModules
  ++ renderOtherModules libraryOtherModules
  ++ renderDependencies libraryDependencies
  ++ renderDefaultExtensions libraryDefaultExtensions
  ++ renderGhcOptions libraryGhcOptions
  ++ "  default-language: Haskell2010\n"


renderSourceDirs :: [String] -> String
renderSourceDirs dirs
  | null dirs = ""
  | otherwise = "  hs-source-dirs: " ++ intercalate ", " dirs ++ "\n"

renderExposedModules :: [String] -> String
renderExposedModules modules
  | null modules = ""
  | otherwise = "  exposed-modules:\n" ++ (unlines $ map ("      " ++) modules)

renderOtherModules :: [String] -> String
renderOtherModules modules
  | null modules = ""
  | otherwise = "  other-modules:\n" ++ (unlines $ map ("      " ++) modules)

renderDependencies :: [[Dependency]] -> String
renderDependencies dependencies
  | null dependencies = ""
  | otherwise = concatMap render $ zip (True : repeat False) dependencies
  where
    render (isFirst, xs)
      | isFirst = "  build-depends:\n      " ++ intercalate "\n    , " xs ++ "\n"
      | otherwise = "\n    , " ++ intercalate "\n    , " xs ++ "\n"

renderGhcOptions :: [GhcOption] -> String
renderGhcOptions ghcOptions
  | null ghcOptions = ""
  | otherwise = "  ghc-options: " ++ unwords ghcOptions ++ "\n"

renderDefaultExtensions :: [String] -> String
renderDefaultExtensions defaultExtensions
  | null defaultExtensions = ""
  | otherwise = "  default-extensions: " ++ unwords defaultExtensions ++ "\n"
