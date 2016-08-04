{-# LANGUAGE OverloadedStrings #-}
module Hpack.ConvertSpec (spec) where

import           Prelude ()
import           Prelude.Compat

import           Control.Monad
import qualified Data.ByteString as ByteString hiding (pack, unpack)
import qualified Data.ByteString.Char8 as ByteString (unpack)
import           System.Directory
import           System.FilePath
import           Test.Hspec

import           Hpack.Config
import           Hpack.Convert

spec :: Spec
spec =
  describe "fromPackageDescriptionString" $ do
    describe "generated from ./test/data/*.{cabal,yaml}" $ do
      cabalFiles <- runIO $ do
        fs <- getDirectoryContents "./test/data"
        return $ map ("./test/data" </>)
          (filter ((== ".cabal") . takeExtension) fs)

      forM_ cabalFiles $ \cabalFp -> do
        let expectationFp = cabalFp ++ ".yaml"
            readPackageFromCabal fp = do
              cc <- readFile fp
              let Right pkg = fromPackageDescriptionString cc
              return pkg
        expectationExists <- runIO $ doesFileExist expectationFp

        if not expectationExists
          then do
            it ("parses " ++ cabalFp)
              (pendingWith ("no expected output file at " ++ expectationFp))
            it ("produces the same output from " ++ cabalFp ++ " as from " ++ expectationFp)
              (pendingWith ("no expected output file at " ++ expectationFp))
          else do
            it ("parses " ++ cabalFp) $ do
              pkg <- readPackageFromCabal cabalFp
              ecc <- ByteString.readFile expectationFp
              -- ByteString.writeFile (cabalFp ++ ".yaml.out") bpkg
              encodePackage pkg `shouldBe` ecc

            it ("produces the same output from " ++ cabalFp ++ " as from " ++ expectationFp) $ do
              pkg <- readPackageFromCabal cabalFp
              Right (_, pkg') <- readPackageConfig expectationFp
              -- ByteString.writeFile (cabalFp ++ ".yaml.out.2") (encodePackage pkg')

              -- This is here to make sure encoding is consistent with the cabal
              -- file encoding
              ByteString.unpack (encodePackage pkg') `shouldBe` ByteString.unpack (encodePackage pkg)

    describe "simple generated cabal file" $ do
      it "cabal init -m" $ do
        pkgDescription <- readFile "./test/data/cabal-init-minimal.cabal"
        let pkg = fromPackageDescriptionString pkgDescription
        pkg `shouldBe` Right Package { packageName = "cabal-init-minimal"
                                     , packageVersion = "0.1.0.0"
                                     , packageSynopsis = Nothing
                                     , packageDescription = Nothing
                                     , packageHomepage = Nothing
                                     , packageBugReports = Nothing
                                     , packageCategory = Nothing
                                     , packageStability = Nothing
                                     , packageAuthor = [ "Pedro Tacla Yamada" ]
                                     , packageMaintainer = [ "tacla.yamada@gmail.com" ]
                                     , packageCopyright = []
                                     , packageLicense = Just "PublicDomain"
                                     , packageLicenseFile = Nothing
                                     , packageTestedWith = Nothing
                                     , packageFlags = []
                                     , packageExtraSourceFiles = ["ChangeLog.md"]
                                     , packageDataFiles = []
                                     , packageSourceRepository = Nothing
                                     , packageLibrary = Just Section { sectionData = Library { libraryExposed = Just True
                                                                                             , libraryExposedModules = []
                                                                                             , libraryOtherModules = []
                                                                                             , libraryReexportedModules = []
                                                                                             }
                                                                     , sectionSourceDirs = ["src"]
                                                                     , sectionDependencies = [ Dependency "base >=4.8 && <4.9" Nothing ]
                                                                     , sectionDefaultExtensions = []
                                                                     , sectionOtherExtensions = []
                                                                     , sectionGhcOptions = []
                                                                     , sectionGhcProfOptions = []
                                                                     , sectionCppOptions = []
                                                                     , sectionCCOptions = []
                                                                     , sectionCSources = []
                                                                     , sectionExtraLibDirs = []
                                                                     , sectionExtraLibraries = []
                                                                     , sectionIncludeDirs = []
                                                                     , sectionInstallIncludes = []
                                                                     , sectionLdOptions = []
                                                                     , sectionBuildable = Just True
                                                                     , sectionConditionals = []
                                                                     , sectionBuildTools = []
                                                                     }
                                     , packageExecutables = []
                                     , packageTests = []
                                     , packageBenchmarks = []
                                     }

      it "cabal init -m with executables" $ do
        pkgDescription <- readFile "./test/data/cabal-init-with-executables.cabal"
        let pkg = fromPackageDescriptionString pkgDescription
        pkg `shouldBe` Right Package { packageName = "cabal-init-minimal"
                                     , packageVersion = "0.1.0.0"
                                     , packageSynopsis = Nothing
                                     , packageDescription = Nothing
                                     , packageHomepage = Nothing
                                     , packageBugReports = Nothing
                                     , packageCategory = Nothing
                                     , packageStability = Nothing
                                     , packageAuthor = [ "Pedro Tacla Yamada" ]
                                     , packageMaintainer = [ "tacla.yamada@gmail.com" ]
                                     , packageCopyright = []
                                     , packageLicense = Just "PublicDomain"
                                     , packageLicenseFile = Nothing
                                     , packageTestedWith = Nothing
                                     , packageFlags = []
                                     , packageExtraSourceFiles = ["ChangeLog.md"]
                                     , packageDataFiles = []
                                     , packageSourceRepository = Nothing
                                     , packageLibrary = Nothing
                                     , packageExecutables = [
                                             Section { sectionData = Executable { executableName = "hello-world"
                                                                                , executableMain = "HelloWorld.hs"
                                                                                , executableOtherModules = []
                                                                                }
                                                     , sectionSourceDirs = [ "src" ]
                                                     , sectionDependencies = [ Dependency "base >=4.8 && <4.9" Nothing ]
                                                     , sectionDefaultExtensions = []
                                                     , sectionOtherExtensions = []
                                                     , sectionGhcOptions = []
                                                     , sectionGhcProfOptions = []
                                                     , sectionCppOptions = []
                                                     , sectionCCOptions = []
                                                     , sectionCSources = []
                                                     , sectionExtraLibDirs = []
                                                     , sectionExtraLibraries = []
                                                     , sectionIncludeDirs = []
                                                     , sectionInstallIncludes = []
                                                     , sectionLdOptions = []
                                                     , sectionBuildable = Just True
                                                     , sectionConditionals = []
                                                     , sectionBuildTools = []
                                                     }
                                             ]
                                     , packageTests = []
                                     , packageBenchmarks = []
                                     }

      it "cabal init -m with test-suites" $ do
        pkgDescription <- readFile "./test/data/cabal-init-with-tests.cabal"
        let pkg = fromPackageDescriptionString pkgDescription
        pkg `shouldBe` Right Package { packageName = "cabal-init-minimal"
                                     , packageVersion = "0.1.0.0"
                                     , packageSynopsis = Nothing
                                     , packageDescription = Nothing
                                     , packageHomepage = Nothing
                                     , packageBugReports = Nothing
                                     , packageCategory = Nothing
                                     , packageStability = Nothing
                                     , packageAuthor = [ "Pedro Tacla Yamada" ]
                                     , packageMaintainer = [ "tacla.yamada@gmail.com" ]
                                     , packageCopyright = []
                                     , packageLicense = Just "PublicDomain"
                                     , packageLicenseFile = Nothing
                                     , packageTestedWith = Nothing
                                     , packageFlags = []
                                     , packageExtraSourceFiles = ["ChangeLog.md"]
                                     , packageDataFiles = []
                                     , packageSourceRepository = Nothing
                                     , packageLibrary = Nothing
                                     , packageExecutables = [
                                             Section { sectionData = Executable { executableName = "hello-world"
                                                                                , executableMain = "HelloWorld.hs"
                                                                                , executableOtherModules = []
                                                                                }
                                                     , sectionSourceDirs = [ "src" ]
                                                     , sectionDependencies = [ Dependency "base >=4.8 && <4.9" Nothing ]
                                                     , sectionDefaultExtensions = []
                                                     , sectionOtherExtensions = []
                                                     , sectionGhcOptions = []
                                                     , sectionGhcProfOptions = []
                                                     , sectionCppOptions = []
                                                     , sectionCCOptions = []
                                                     , sectionCSources = []
                                                     , sectionExtraLibDirs = []
                                                     , sectionExtraLibraries = []
                                                     , sectionIncludeDirs = []
                                                     , sectionInstallIncludes = []
                                                     , sectionLdOptions = []
                                                     , sectionBuildable = Just True
                                                     , sectionConditionals = []
                                                     , sectionBuildTools = []
                                                     }
                                             ]
                                     , packageTests = [
                                             Section { sectionData = Executable { executableName = "hello-world-spec"
                                                                                , executableMain = "Spec.hs"
                                                                                , executableOtherModules = []
                                                                                }
                                                     , sectionSourceDirs = [ "src", "test" ]
                                                     , sectionDependencies = [ Dependency "base >=4.8 && <4.9" Nothing ]
                                                     , sectionDefaultExtensions = []
                                                     , sectionOtherExtensions = []
                                                     , sectionGhcOptions = []
                                                     , sectionGhcProfOptions = []
                                                     , sectionCppOptions = []
                                                     , sectionCCOptions = []
                                                     , sectionCSources = []
                                                     , sectionExtraLibDirs = []
                                                     , sectionExtraLibraries = []
                                                     , sectionIncludeDirs = []
                                                     , sectionInstallIncludes = []
                                                     , sectionLdOptions = []
                                                     , sectionBuildable = Just True
                                                     , sectionConditionals = []
                                                     , sectionBuildTools = []
                                                     }
                                                      ]
                                     , packageBenchmarks = []
                                     }

      it "cabal init -m with benchmarks" $ do
        pkgDescription <- readFile "./test/data/cabal-init-with-benchmarks.cabal"
        let pkg = fromPackageDescriptionString pkgDescription
        pkg `shouldBe` Right Package { packageName = "cabal-init-minimal"
                                     , packageVersion = "0.1.0.0"
                                     , packageSynopsis = Nothing
                                     , packageDescription = Nothing
                                     , packageHomepage = Nothing
                                     , packageBugReports = Nothing
                                     , packageCategory = Nothing
                                     , packageStability = Nothing
                                     , packageAuthor = [ "Pedro Tacla Yamada" ]
                                     , packageMaintainer = [ "tacla.yamada@gmail.com" ]
                                     , packageCopyright = []
                                     , packageLicense = Just "PublicDomain"
                                     , packageLicenseFile = Nothing
                                     , packageTestedWith = Nothing
                                     , packageFlags = []
                                     , packageExtraSourceFiles = ["ChangeLog.md"]
                                     , packageDataFiles = []
                                     , packageSourceRepository = Nothing
                                     , packageLibrary = Nothing
                                     , packageExecutables = [
                                             Section { sectionData = Executable { executableName = "hello-world"
                                                                                , executableMain = "HelloWorld.hs"
                                                                                , executableOtherModules = []
                                                                                }
                                                     , sectionSourceDirs = [ "src" ]
                                                     , sectionDependencies = [ Dependency "base >=4.8 && <4.9" Nothing ]
                                                     , sectionDefaultExtensions = []
                                                     , sectionOtherExtensions = []
                                                     , sectionGhcOptions = []
                                                     , sectionGhcProfOptions = []
                                                     , sectionCppOptions = []
                                                     , sectionCCOptions = []
                                                     , sectionCSources = []
                                                     , sectionExtraLibDirs = []
                                                     , sectionExtraLibraries = []
                                                     , sectionIncludeDirs = []
                                                     , sectionInstallIncludes = []
                                                     , sectionLdOptions = []
                                                     , sectionBuildable = Just True
                                                     , sectionConditionals = []
                                                     , sectionBuildTools = []
                                                     }
                                             ]
                                     , packageTests = [
                                             Section { sectionData = Executable { executableName = "hello-world-spec"
                                                                                , executableMain = "Spec.hs"
                                                                                , executableOtherModules = []
                                                                                }
                                                     , sectionSourceDirs = [ "src", "test" ]
                                                     , sectionDependencies = [ Dependency "base >=4.8 && <4.9" Nothing ]
                                                     , sectionDefaultExtensions = []
                                                     , sectionOtherExtensions = []
                                                     , sectionGhcOptions = []
                                                     , sectionGhcProfOptions = []
                                                     , sectionCppOptions = []
                                                     , sectionCCOptions = []
                                                     , sectionCSources = []
                                                     , sectionExtraLibDirs = []
                                                     , sectionExtraLibraries = []
                                                     , sectionIncludeDirs = []
                                                     , sectionInstallIncludes = []
                                                     , sectionLdOptions = []
                                                     , sectionBuildable = Just True
                                                     , sectionConditionals = []
                                                     , sectionBuildTools = []
                                                     }
                                                      ]
                                     , packageBenchmarks = [
                                             Section { sectionData = Executable { executableName = "hello-world-benchmark"
                                                                                , executableMain = "Bench.hs"
                                                                                , executableOtherModules = []
                                                                                }
                                                     , sectionSourceDirs = [ "src", "benchmarks" ]
                                                     , sectionDependencies = [ Dependency "base >=4.8 && <4.9" Nothing ]
                                                     , sectionDefaultExtensions = []
                                                     , sectionOtherExtensions = []
                                                     , sectionGhcOptions = []
                                                     , sectionGhcProfOptions = []
                                                     , sectionCppOptions = []
                                                     , sectionCCOptions = []
                                                     , sectionCSources = []
                                                     , sectionExtraLibDirs = []
                                                     , sectionExtraLibraries = []
                                                     , sectionIncludeDirs = []
                                                     , sectionInstallIncludes = []
                                                     , sectionLdOptions = []
                                                     , sectionBuildable = Just True
                                                     , sectionConditionals = []
                                                     , sectionBuildTools = []
                                                     }
                                             ]
                                     }

      it "cabal init -m with conditionals" $ do
        pkgDescription <- readFile "./test/data/cabal-init-with-conditionals.cabal"
        let pkg = fromPackageDescriptionString pkgDescription
        pkg `shouldBe` Right Package { packageName = "cabal-init-minimal"
                                     , packageVersion = "0.1.0.0"
                                     , packageSynopsis = Nothing
                                     , packageDescription = Nothing
                                     , packageHomepage = Nothing
                                     , packageBugReports = Nothing
                                     , packageCategory = Nothing
                                     , packageStability = Nothing
                                     , packageAuthor = [ "Pedro Tacla Yamada" ]
                                     , packageMaintainer = [ "tacla.yamada@gmail.com" ]
                                     , packageCopyright = []
                                     , packageLicense = Just "PublicDomain"
                                     , packageLicenseFile = Nothing
                                     , packageTestedWith = Nothing
                                     , packageFlags = []
                                     , packageExtraSourceFiles = ["ChangeLog.md"]
                                     , packageDataFiles = []
                                     , packageSourceRepository = Nothing
                                     , packageLibrary = Nothing
                                     , packageExecutables = [
                                             Section { sectionData = Executable { executableName = "hello-world"
                                                                                , executableMain = "HelloWorld.hs"
                                                                                , executableOtherModules = []
                                                                                }
                                                     , sectionSourceDirs = [ "src" ]
                                                     , sectionDependencies = [ Dependency "base >=4.8 && <4.9" Nothing ]
                                                     , sectionDefaultExtensions = []
                                                     , sectionOtherExtensions = []
                                                     , sectionGhcOptions = []
                                                     , sectionGhcProfOptions = []
                                                     , sectionCppOptions = []
                                                     , sectionCCOptions = []
                                                     , sectionCSources = []
                                                     , sectionExtraLibDirs = []
                                                     , sectionExtraLibraries = []
                                                     , sectionIncludeDirs = []
                                                     , sectionInstallIncludes = []
                                                     , sectionLdOptions = []
                                                     , sectionBuildable = Just True
                                                     , sectionConditionals = [ Conditional "os(osx)" (emptySection {sectionBuildable = Just False}) Nothing
                                                                             ]
                                                     , sectionBuildTools = []
                                                     }
                                             ]
                                     , packageTests = []
                                     , packageBenchmarks = []
                                     }

      it "cabal init -m with conditionals and an else branch" $ do
        pkgDescription <- readFile "./test/data/cabal-init-with-conditionals-and-else.cabal"
        let pkg = fromPackageDescriptionString pkgDescription
        pkg `shouldBe` Right Package { packageName = "cabal-init-minimal"
                                     , packageVersion = "0.1.0.0"
                                     , packageSynopsis = Nothing
                                     , packageDescription = Nothing
                                     , packageHomepage = Nothing
                                     , packageBugReports = Nothing
                                     , packageCategory = Nothing
                                     , packageStability = Nothing
                                     , packageAuthor = [ "Pedro Tacla Yamada" ]
                                     , packageMaintainer = [ "tacla.yamada@gmail.com" ]
                                     , packageCopyright = []
                                     , packageLicense = Just "PublicDomain"
                                     , packageLicenseFile = Nothing
                                     , packageTestedWith = Nothing
                                     , packageFlags = []
                                     , packageExtraSourceFiles = ["ChangeLog.md"]
                                     , packageDataFiles = []
                                     , packageSourceRepository = Nothing
                                     , packageLibrary = Nothing
                                     , packageExecutables = [
                                             Section { sectionData = Executable { executableName = "hello-world"
                                                                                , executableMain = "HelloWorld.hs"
                                                                                , executableOtherModules = []
                                                                                }
                                                     , sectionSourceDirs = [ "src" ]
                                                     , sectionDependencies = [ Dependency "base >=4.8 && <4.9" Nothing ]
                                                     , sectionDefaultExtensions = []
                                                     , sectionOtherExtensions = []
                                                     , sectionGhcOptions = []
                                                     , sectionGhcProfOptions = []
                                                     , sectionCppOptions = []
                                                     , sectionCCOptions = []
                                                     , sectionCSources = []
                                                     , sectionExtraLibDirs = []
                                                     , sectionExtraLibraries = []
                                                     , sectionIncludeDirs = []
                                                     , sectionInstallIncludes = []
                                                     , sectionLdOptions = []
                                                     , sectionBuildable = Just True
                                                     , sectionConditionals = [
                                                             Conditional "os(osx)"
                                                             (emptySection {sectionSourceDirs = ["osx"]})
                                                             (Just emptySection {sectionSourceDirs = ["notosx"]})
                                                             ]
                                                     , sectionBuildTools = []
                                                     }
                                             ]
                                     , packageTests = []
                                     , packageBenchmarks = []
                                     }

emptySection :: Section ()
emptySection = Section { sectionData = ()
                       , sectionSourceDirs = []
                       , sectionDependencies = []
                       , sectionDefaultExtensions = []
                       , sectionOtherExtensions = []
                       , sectionGhcOptions = []
                       , sectionGhcProfOptions = []
                       , sectionCppOptions = []
                       , sectionCCOptions = []
                       , sectionCSources = []
                       , sectionExtraLibDirs = []
                       , sectionExtraLibraries = []
                       , sectionIncludeDirs = []
                       , sectionInstallIncludes = []
                       , sectionLdOptions = []
                       , sectionBuildable = Just True
                       , sectionConditionals = []
                       , sectionBuildTools = []
                       }
