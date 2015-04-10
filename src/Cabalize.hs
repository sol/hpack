{-# LANGUAGE QuasiQuotes, RecordWildCards #-}
module Cabalize (
  cabalize
-- exported for testing
, renderPackage
) where

import           Control.Applicative
import           Data.Maybe
import           Data.List
import           System.Exit.Compat

import           Util
import           Config

configFile :: FilePath
configFile = "package.yaml"

cabalize :: IO (FilePath, String)
cabalize = do
  mPackage <- readConfig configFile
  case mPackage of
    Right package -> do
      let cabalFile = packageName package ++ ".cabal"

      old <- tryReadFile cabalFile

      let alignment = fromMaybe 16 (old >>= sniffAlignment)
          output = concat [
              "-- This file has been generated from " ++ configFile ++ " by Cabalize.\n"
            , "--\n"
            , "-- see: https://github.com/sol/cabalize\n"
            , "\n"
            , renderPackage alignment (maybe [] extractFieldOrderHint old) package
            ]
      return (cabalFile, output)
    Left err -> die err

renderPackage :: Int -> [String] -> Package -> String
renderPackage alignment existingFieldOrder Package{..} = unlines output ++ renderExecutables packageExecutables ++ renderTests packageTests
  where
    padding name = replicate (alignment - length name - 2) ' '

    formatField :: (String, String) -> String
    formatField (name, value) = name ++ separator ++ value
      where
        separator
          | "\n" `isPrefixOf` value = ":"
          | otherwise = ": " ++ padding name

    output = map formatField sortedFields ++ catMaybes [
        sourceRepository <$> packageSourceRepository
      , renderLibrary <$> packageLibrary
      ]

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
      , ("description", (normalizeDescription <$> packageDescription))
      , ("category", packageCategory)
      , ("author", packageAuthor)
      , ("maintainer", packageMaintainer)
      , ("copyright", packageCopyright)
      , ("license", packageLicense)
      , ("license-file", packageLicenseFile)
      , ("build-type", Just "Simple")
      , ("cabal-version", Just ">= 1.10")
      ]

    defaultFieldOrder :: [String]
    defaultFieldOrder = map fst fields

    normalizeDescription = intercalate "\n  ." . map ("\n  " ++) . lines

    sourceRepository :: String -> String
    sourceRepository = ("\nsource-repository head\n  type: git\n  location: " ++)

renderExecutables :: [Executable] -> String
renderExecutables = intercalate "\n" . map renderExecutable

renderExecutable :: Executable -> String
renderExecutable executable@Executable{..} =
     "executable "
  ++ executableName ++ "\n"
  ++ renderExecutableSection executable

renderTests :: [Executable] -> String
renderTests = intercalate "\n" . map renderTest

renderTest :: Executable -> String
renderTest executable@Executable{..} =
     "test-suite " ++ executableName ++ "\n"
  ++ "  type: exitcode-stdio-1.0\n"
  ++ renderExecutableSection executable

renderExecutableSection :: Executable -> String
renderExecutableSection Executable{..} = 
     renderSourceDirs executableSourceDirs
  ++ "  main-is: " ++ executableMain ++ "\n"
  ++ renderDependencies executableDependencies 
  ++ renderGhcOptions executableGhcOptions
  ++ "  default-language: Haskell2010\n"

renderLibrary :: Library -> String
renderLibrary Library{..} = unlines [
    "library"
  , "  hs-source-dirs: src"
  ]
  ++ renderExposedModules libraryExposedModules
  ++ renderOtherModules libraryOtherModules
  ++ renderDependencies libraryDependencies
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
