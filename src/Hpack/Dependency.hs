{-# LANGUAGE CPP #-}
module Hpack.Dependency (
  parseDependency
) where

import           Prelude ()
import           Prelude.Compat

import           Text.PrettyPrint (renderStyle, Style(..), Mode(..))
import           Control.Monad
import qualified Distribution.Compat.ReadP as D
import qualified Distribution.Package as D
import qualified Distribution.Text as D
import qualified Distribution.Version as D

dependencyName :: D.Dependency -> String
#if MIN_VERSION_Cabal(2,0,0)
dependencyName = D.unPackageName . D.depPkgName
#else
dependencyName (D.Dependency (D.PackageName name) _) = name
#endif

dependencyVersionRange :: D.Dependency -> D.VersionRange
#if MIN_VERSION_Cabal(2,0,0)
dependencyVersionRange = D.depVerRange
#else
dependencyVersionRange (D.Dependency _ versionRange) = versionRange
#endif

parseDependency :: Monad m => String -> m (String, Maybe String)
parseDependency = liftM render . parseCabalDependency
  where
    render :: D.Dependency -> (String, Maybe String)
    render d = (name, range)
      where
        name = dependencyName d
        versionRange = dependencyVersionRange d

        range
          | D.isAnyVersion versionRange = Nothing
          | otherwise = Just . renderStyle style . D.disp $ versionRange
          where
            style = Style OneLineMode 0 0

parseCabalDependency :: Monad m => String -> m D.Dependency
parseCabalDependency s = case [d | (d, "") <- D.readP_to_S D.parse s] of
  [d] -> return d
  _ -> fail $ "invalid dependency " ++ show s
