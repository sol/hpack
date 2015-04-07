{-# LANGUAGE QuasiQuotes, RecordWildCards #-}
module Cabalize (
  cabalize
-- exported for testing
, renderPackage
) where

import           Prelude ()
import           Prelude.Compat

import           Data.Maybe
import           Data.List (sort, sortBy, elemIndex, intercalate, isPrefixOf)
import           Data.String.Interpolate
import           System.Exit.Compat

import           Util
import           Config

renderExecutables :: [Executable] -> String
renderExecutables = intercalate "\n" . map renderExecutable

renderExecutable :: Executable -> String
renderExecutable executable@Executable{..} = stripEmptyLines [i|
executable #{executableName}
|] ++ renderExecutableSection executable

renderTests :: [Executable] -> String
renderTests = intercalate "\n" . map renderTest

renderTest :: Executable -> String
renderTest executable@Executable{..} = stripEmptyLines [i|
test-suite #{executableName}
  type: exitcode-stdio-1.0
|] ++ renderExecutableSection executable


renderExecutableSection :: Executable -> String
renderExecutableSection Executable{..} = stripEmptyLines [i|
#{if null executableSourceDirs then "" else "  hs-source-dirs: " ++ intercalate ", " executableSourceDirs}
  main-is: #{executableMain}
  build-depends:
      #{intercalate "\n    , " $ sort executableDependencies}
  ghc-options: #{unwords executableGhcOptions}
  default-language: Haskell2010
|]

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

    sortedFields = sortBy orderingForExistingFields fields

    orderingForExistingFields :: (String, a) -> (String, a) -> Ordering
    orderingForExistingFields (key1, _) (key2, _) = index key1 `compare` index key2
      where
        index :: String -> Maybe Int
        index = (`elemIndex` existingFieldOrder)

    fields :: [(String, String)]
    fields = catMaybes . map (\(name, value) -> (,) name <$> value) $ [
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

    normalizeDescription = intercalate "\n  ." . map ("\n  " ++) . lines

    sourceRepository :: String -> String
    sourceRepository = ("\nsource-repository head\n  type: git\n  location: " ++)

renderLibrary :: Library -> String
renderLibrary Library{..} = [i|
library
  hs-source-dirs: src
  exposed-modules:
#{intercalate "\n" . map ("      " ++) $ libraryExposedModules}
  other-modules:
#{intercalate "\n" . map ("      " ++) $ libraryOtherModules}
  build-depends:
      #{intercalate "\n    , " $ sort libraryDependencies}
  ghc-options: #{unwords libraryGhcOptions}
  default-language: Haskell2010
|]
