{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
module Hpack.Convert where

import           Control.Applicative
import           Data.Maybe
import qualified Data.Version as Version
import qualified Distribution.Compiler as Compiler
import qualified Distribution.InstalledPackageInfo as Cabal
import qualified Distribution.Package as Cabal
import qualified Distribution.PackageDescription as Cabal
import qualified Distribution.PackageDescription.Parse as Cabal
import qualified Distribution.Text as Cabal
import qualified Distribution.Version as Cabal
import           Hpack.Config  hiding (package)
import           Text.PrettyPrint (fsep, (<+>))

-- * Public API

-- | Reads a 'Package' from cabal's 'GenericPackageDescription' representation
-- of a @.cabal@ file
fromPackageDescription :: Cabal.GenericPackageDescription -> Package
fromPackageDescription Cabal.GenericPackageDescription{..} =
    let Cabal.PackageDescription{..} = packageDescription
    in
    Package { packageName = Cabal.unPackageName (Cabal.pkgName package)
            , packageVersion = Version.showVersion (Cabal.pkgVersion package)
            , packageSynopsis = nullNothing synopsis
            , packageDescription = nullNothing description
            , packageHomepage = nullNothing homepage
            , packageBugReports = nullNothing bugReports
            , packageCategory = nullNothing category
            , packageStability = nullNothing stability
            , packageAuthor = maybeToList (nullNothing author)
            , packageMaintainer = maybeToList (nullNothing maintainer)
            , packageCopyright = maybeToList (nullNothing copyright)
            , packageLicense = Just (show (Cabal.disp license))
            , packageLicenseFile = listToMaybe licenseFiles
            , packageTestedWith =
                    show .
                    fsep . map (\(f, vr) -> Cabal.disp f <+> Cabal.disp vr ) <$>
                    nullNothing testedWith
            , packageFlags =
                    map (\Cabal.MkFlag{..} ->
                             let Cabal.FlagName fn = flagName
                             in Flag { flagName = fn
                                     , flagDescription =
                                             nullNothing flagDescription
                                     , flagManual = flagManual
                                     , flagDefault = flagDefault
                                     })
                    genPackageFlags
            , packageExtraSourceFiles = extraSrcFiles
            , packageDataFiles = dataFiles
            , packageSourceRepository = fromSourceRepos sourceRepos
            , packageLibrary = fromCondLibrary condLibrary
            , packageExecutables = fromCondExecutables condExecutables
            , packageTests = fromCondTestSuites condTestSuites
            , packageBenchmarks = fromCondBenchmarks condBenchmarks
            }

-- | Reads a 'Package' from a @.cabal@ manifest string
fromPackageDescriptionString :: String -> Either ConvertError Package
fromPackageDescriptionString pkgStr =
    case Cabal.parsePackageDescription pkgStr of
        Cabal.ParseFailed e -> Left (ConvertCabalParseError e)
        Cabal.ParseOk _ gpkg -> Right (fromPackageDescription gpkg)

data ConvertError = ConvertCabalParseError Cabal.PError
  deriving(Show, Eq)

-- data ConvertWarning = CWIgnoreSection String
--                     | CWIgnoreCondition String
--                     | CWIgnoreSourceRepo Cabal.SourceRepo
--                     | CWSourceRepoWithoutUrl Cabal.SourceRepo

-- * Private functions for converting each section

fromSourceRepos :: [Cabal.SourceRepo] -> Maybe SourceRepository
fromSourceRepos [] = Nothing
fromSourceRepos (_repo@Cabal.SourceRepo{..}:_more) =
    -- (
    Just SourceRepository { sourceRepositoryUrl = fromMaybe "" repoLocation
                          -- TODO - this is broken (?)
                          , sourceRepositorySubdir = repoSubdir
                          }
    -- TODO - Warnings
    -- , case repoLocation of
    --       Nothing -> [CWSourceRepoWithoutUrl repo]
    --       _ -> []
    --   ++ map CWIgnoreSourceRepo more
    -- )

fromDependency :: Cabal.Dependency -> Dependency
fromDependency (Cabal.Dependency pn Cabal.AnyVersion) =
    Dependency (show (Cabal.disp pn)) Nothing
fromDependency (Cabal.Dependency pn vr) =
    Dependency (show (Cabal.disp pn <+> Cabal.disp vr)) Nothing

fromCondLibrary :: Maybe (Cabal.CondTree Cabal.ConfVar [Cabal.Dependency] Cabal.Library) -> Maybe (Section Library)
fromCondLibrary mcondLibrary = do
    condLibrary@(Cabal.CondNode Cabal.Library{libBuildInfo} _ components) <- mcondLibrary
    l <- libFromCondLibrary condLibrary
    return (sectionWithBuildInfo l libBuildInfo)
        { sectionConditionals = map fromCondComponentHasBuildInfo components
        }

fromCondExecutables :: [(String, Cabal.CondTree Cabal.ConfVar [Cabal.Dependency] Cabal.Executable)] -> [Section Executable]
fromCondExecutables = map fromCondExecutableTup

fromCondTestSuites :: [(String, Cabal.CondTree Cabal.ConfVar [Cabal.Dependency] Cabal.TestSuite)] -> [Section Executable]
fromCondTestSuites = mapMaybe fromCondTestSuiteTup

fromCondBenchmarks :: [(String, Cabal.CondTree Cabal.ConfVar [Cabal.Dependency] Cabal.Benchmark)] -> [Section Executable]
fromCondBenchmarks = mapMaybe fromCondBenchmarkTup

fromCondExecutableTup :: (String, Cabal.CondTree Cabal.ConfVar [Cabal.Dependency] Cabal.Executable) -> Section Executable
fromCondExecutableTup etup@(_, Cabal.CondNode Cabal.Executable{buildInfo} _ components) =
    let e = exeFromCondExecutableTup etup
    in (sectionWithBuildInfo e buildInfo)
       { sectionConditionals = map fromCondComponentHasBuildInfo components
       }

fromCondTestSuiteTup :: (String, Cabal.CondTree Cabal.ConfVar [Cabal.Dependency] Cabal.TestSuite) -> Maybe (Section Executable)
fromCondTestSuiteTup ttup@(_, Cabal.CondNode Cabal.TestSuite{testBuildInfo} _ components) = do
    te <- testExeFromCondExecutableTup ttup
    return (sectionWithBuildInfo te testBuildInfo)
       { sectionConditionals = map fromCondComponentHasBuildInfo components
       }

fromCondBenchmarkTup :: (String, Cabal.CondTree Cabal.ConfVar [Cabal.Dependency] Cabal.Benchmark) -> Maybe (Section Executable)
fromCondBenchmarkTup btup@(_, Cabal.CondNode Cabal.Benchmark{benchmarkBuildInfo} _ components) = do
    be <- benchExeFromCondExecutableTup btup
    return (sectionWithBuildInfo be benchmarkBuildInfo)
       { sectionConditionals = map fromCondComponentHasBuildInfo components
       }

-- * Conditional Mapping
class HasBuildInfo a where
    getBuildInfo :: a -> Cabal.BuildInfo

instance HasBuildInfo Cabal.Library where
    getBuildInfo Cabal.Library{libBuildInfo} = libBuildInfo

instance HasBuildInfo Cabal.Executable where
    getBuildInfo Cabal.Executable{buildInfo} = buildInfo

instance HasBuildInfo Cabal.TestSuite where
    getBuildInfo Cabal.TestSuite{testBuildInfo} = testBuildInfo

instance HasBuildInfo Cabal.Benchmark where
    getBuildInfo Cabal.Benchmark{benchmarkBuildInfo} = benchmarkBuildInfo

fromCondHasBuildInfo :: HasBuildInfo a => Cabal.CondTree Cabal.ConfVar [Cabal.Dependency] a -> Section ()
fromCondHasBuildInfo (Cabal.CondNode hbi _ components) =
    let bi = getBuildInfo hbi
    in (sectionWithBuildInfo () bi)
       { sectionConditionals = map fromCondComponentHasBuildInfo components
       }

fromCondComponentHasBuildInfo :: (HasBuildInfo a)
    => ( Cabal.Condition Cabal.ConfVar
       , Cabal.CondTree Cabal.ConfVar [Cabal.Dependency] a
       , Maybe (Cabal.CondTree Cabal.ConfVar [Cabal.Dependency] a)
       )
    -> Conditional
fromCondComponentHasBuildInfo (cond, ifTree, elseTree) =
    Conditional { conditionalCondition = fromCondition cond
                , conditionalThen = fromCondHasBuildInfo ifTree
                , conditionalElse = fromCondHasBuildInfo <$> elseTree
                }

fromCondition :: Cabal.Condition Cabal.ConfVar -> String
fromCondition (Cabal.Var c) = case c of
    Cabal.OS os -> "os(" ++ show (Cabal.disp os) ++ ")"
    Cabal.Flag (Cabal.FlagName fl) -> "flag(" ++ fl ++ ")"
    Cabal.Arch ar -> "arch(" ++ show (Cabal.disp ar) ++ ")"
    Cabal.Impl cc vr -> "impl(" ++ show (Cabal.disp cc <+> Cabal.disp vr)  ++ ")"
fromCondition (Cabal.CNot c) = "!(" ++ fromCondition c ++ ")"
fromCondition (Cabal.COr c1 c2) = "(" ++ fromCondition c1 ++ ") || (" ++ fromCondition c2 ++ ")"
fromCondition (Cabal.CAnd c1 c2) = "(" ++ fromCondition c1 ++ ") && (" ++ fromCondition c2 ++ ")"
fromCondition (Cabal.Lit b) = show b


-- * Private helpers

-- | Builds a 'Package' 'Section' from a data entity and a 'BuildInfo' entity
sectionWithBuildInfo :: a -> Cabal.BuildInfo -> Section a
sectionWithBuildInfo d Cabal.BuildInfo{..} =
    Section { sectionData = d
            , sectionSourceDirs = hsSourceDirs
            , sectionDependencies = map fromDependency targetBuildDepends
            , sectionDefaultExtensions = map (show . Cabal.disp)
                                             defaultExtensions
            , sectionOtherExtensions = map (show . Cabal.disp) otherExtensions
            , sectionGhcOptions = fromMaybe [] $
                lookup Compiler.GHC options
            , sectionGhcProfOptions = fromMaybe [] $
                lookup Compiler.GHC profOptions
            , sectionCppOptions = cppOptions
            , sectionCCOptions = ccOptions
            , sectionCSources = cSources
            , sectionExtraLibDirs = extraLibDirs
            , sectionExtraLibraries = extraLibs
            , sectionIncludeDirs = includeDirs
            , sectionInstallIncludes = installIncludes
            , sectionLdOptions = ldOptions
            , sectionBuildable = Just buildable
            -- TODO ^^ ????
            , sectionConditionals = []
            -- TODO ^^ ????
            , sectionBuildTools = map fromDependency buildTools
            }

libFromCondLibrary :: Cabal.CondTree Cabal.ConfVar [Cabal.Dependency] Cabal.Library -> Maybe Library
libFromCondLibrary (Cabal.CondNode (Cabal.Library{..}) _ _) = do
    let Cabal.BuildInfo{..} = libBuildInfo
    return Library { libraryExposed = Just libExposed
                   , libraryExposedModules = map (show . Cabal.disp)
                                                 exposedModules
                   , libraryOtherModules = map (show . Cabal.disp) otherModules
                   , libraryReexportedModules = map (show . Cabal.disp)
                                                    reexportedModules
                   }

exeFromCondExecutableTup :: (String, Cabal.CondTree Cabal.ConfVar [Cabal.Dependency] Cabal.Executable) -> Executable
exeFromCondExecutableTup (name, Cabal.CondNode Cabal.Executable{..} _ _) =
    Executable { executableName = name
               , executableMain = modulePath
               , executableOtherModules = map (show . Cabal.disp)
                                              (Cabal.otherModules buildInfo)
               }

testExeFromCondExecutableTup :: (String, Cabal.CondTree Cabal.ConfVar [Cabal.Dependency] Cabal.TestSuite) -> Maybe Executable
testExeFromCondExecutableTup (name, Cabal.CondNode Cabal.TestSuite{..} _ _) =
    case testInterface of
        Cabal.TestSuiteExeV10 _ mainIs -> Just
            Executable { executableName = name
                       , executableMain = mainIs
                       , executableOtherModules = map (show . Cabal.disp)
                                                  (Cabal.otherModules testBuildInfo)
                       }
        _ -> Nothing

benchExeFromCondExecutableTup :: (String, Cabal.CondTree Cabal.ConfVar [Cabal.Dependency] Cabal.Benchmark) -> Maybe Executable
benchExeFromCondExecutableTup (name, Cabal.CondNode Cabal.Benchmark{..} _ _) =
    case benchmarkInterface of
        Cabal.BenchmarkExeV10 _ mainIs -> Just
            Executable { executableName = name
                       , executableMain = mainIs
                       , executableOtherModules = map (show . Cabal.disp)
                                                  (Cabal.otherModules benchmarkBuildInfo)
                       }
        _ -> Nothing

-- | Returns Nothing if a list is empty and Just the list otherwise
--
-- >>> nullNothing []
-- Nothing
-- >>> nullNothing [1, 2, 3]
-- Just [1, 2, 3]
nullNothing :: [a] -> Maybe [a]
nullNothing s = const s <$> listToMaybe s
