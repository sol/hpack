{-# LANGUAGE DeriveFunctor #-}
module Hpack.License where

import           Control.Arrow ((&&&))

import           Distribution.Pretty (prettyShow)
import qualified Distribution.License as Cabal
import qualified Distribution.SPDX.License as SPDX
import           Distribution.Parsec.Class (eitherParsec)

data License a = DontTouch String | CanSPDX Cabal.License a | MustSPDX a
  deriving (Eq, Show, Functor)

parseLicense :: String -> License SPDX.License
parseLicense license = case lookup license knownLicenses of
  Just l -> CanSPDX l (Cabal.licenseToSPDX l)
  Nothing -> case spdxLicense of
    Just l -> MustSPDX l
    Nothing -> DontTouch license
  where
    knownLicenses :: [(String, Cabal.License)]
    knownLicenses = map (prettyShow &&& id) (Cabal.BSD4 : Cabal.knownLicenses)

    spdxLicense :: Maybe SPDX.License
    spdxLicense  = either (const Nothing) Just (eitherParsec license)
