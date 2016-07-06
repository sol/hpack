{-# LANGUAGE BangPatterns, DeriveGeneric, FlexibleContexts, LambdaCase,
             TupleSections #-}
-- | Conditionally-exposed modules whose availability depends on the
-- presence of other packages in the build plan.
module Hpack.OptionalExposure (
  OptionalExposure(optionalExposureDependencies, optionalExposureModules),
  mkOptionalExposure, optionallyExposed,
  readBuildPlan, BuildPlan(..)) where
import Prelude ()
import Prelude.Compat
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT(ExceptT), runExceptT)
import Data.Aeson (FromJSON(..), fromJSON, genericParseJSON, defaultOptions)
import Data.Aeson.Types (Result(..), fieldLabelModifier)
import Data.Char (isSpace)
import Data.Map (Map)
import qualified Data.Map.Strict as M
import GHC.Generics (Generic)
import Hpack.Util (List(fromList), hyphenize)
import Hpack.VersionRange
import System.IO

data OptionalExposure = OptionalExposure {
  optionalExposureDependencies :: List String
, optionalExposureModules :: List String
, optionalExposureConstraints :: [(PackageName, VersionRange)]
}

instance Show OptionalExposure where
  show (OptionalExposure deps mods _) =
    "OptionalExposure (" ++ show deps ++ ") (" ++ show mods ++ ")"

instance Eq OptionalExposure where
  OptionalExposure deps mods _ == OptionalExposure deps' mods' _ =
    deps == deps' && mods == mods'

-- | An internal data type used for JSON parsing since the
-- OptionalExposure data type carries some derived data.
data SimpleOptionalExposure = SimpleOptionalExposure {
  _simpleOptionalExposureDependencies :: List String
, _simpleOptionalExposureModules :: List String
} deriving Generic

instance FromJSON SimpleOptionalExposure where
  parseJSON = genericParseJSON
                defaultOptions {fieldLabelModifier =
                                  hyphenize "SimpleOptionalExposure"}

instance FromJSON OptionalExposure where
  parseJSON js = case fromJSON js of
                   Success (SimpleOptionalExposure deps mods) ->
                     maybe (fail "Couldn't parse optional-exposure")
                           pure
                           (mkOptionalExposure deps mods)
                   Error e -> fail e

-- | @mkOptionalExposure dependencyNames optionallyExposedModules@
-- builds an 'OptionalExposure' that can be added to a package
-- definition dependent on a build plan (see 'optionallyExposed').
mkOptionalExposure ::  List String -> List String -> Maybe OptionalExposure
mkOptionalExposure deps modules =
  OptionalExposure deps modules <$>
  mapM parseCabalDependency (fromList deps)

newtype PackageName = PackageName (String) deriving (Eq,Ord,Show)

data BuildPlan = MinimalBuildPlan
               | MaximalBuildPlan
               | BuildPlan (Map PackageName Version)
     deriving (Eq, Show)

-- * Cabal Version Ranges

parseCabalDependency :: String -> Maybe (PackageName, VersionRange)
parseCabalDependency ln =
  case break isSpace ln of
    (pkg,v) -> (PackageName pkg,) <$> parseVersionRange v

-- * Optional Exposure

-- | Returns a list of dependencies and exposed modules given a
-- 'BuildPlan' and an 'OptionalExposure'. This acts as a filter on
-- whether or not the 'BuildPlan' satisfies the 'OptionalExposure''s
-- dependencies.
optionallyExposed :: BuildPlan -> OptionalExposure -> ([String], [String])
optionallyExposed MinimalBuildPlan _ = ([], [])
optionallyExposed MaximalBuildPlan oe =
  let deps = fromList $ optionalExposureDependencies oe
      exposeds = fromList $ optionalExposureModules oe
  in (deps, exposeds)
optionallyExposed (BuildPlan bp) oe
  | all available (optionalExposureConstraints oe) =
      let deps = fromList $ optionalExposureDependencies oe
          exposeds = fromList $ optionalExposureModules oe
      in (deps, exposeds)
  | otherwise = ([],[])
  where available (pkgName, vrange) = case M.lookup pkgName bp of
                                              Nothing -> False
                                              Just v -> vrange v

-- * Helpers

unfoldM :: Monad m => m (Maybe a) -> (a -> b -> b) -> b -> m b
unfoldM gen f z = go z
  where go !acc = gen >>= \case
          Nothing -> return acc
          Just x -> go (f x acc)

-- * Build Plan Parsing

-- | Parse \"foo 0.1.5\" into @(PackageName "foo", Version [0,1,5])@.
parseDep :: String -> Either String (PackageName, Version)
parseDep ln =
  case break isSpace ln of
    (pkg, ' ':v) -> case parseVersion v of
                      Just v' -> Right (PackageName pkg, v')
                      Nothing -> Left $ "Couldn't parse version "++ln
    _ -> Left $ "Couldn't parse dependency: " ++ ln

-- | Parse a file in which each line is a package name, a whitespace
-- character, then a version into a 'BuildPlan'.
readBuildPlan :: FilePath -> IO (Either String BuildPlan)
readBuildPlan f =
  withFile f ReadMode $ \h ->
    let getLn = do eof <- hIsEOF h
                   if eof then return Nothing else Just <$> hGetLine h
        parseDep' :: Applicative m
                  => String -> ExceptT String m (PackageName, Version)
        parseDep' = ExceptT . pure . parseDep
    in fmap (fmap BuildPlan) . runExceptT $
       unfoldM (lift getLn >>= traverse parseDep') (uncurry M.insert) mempty
