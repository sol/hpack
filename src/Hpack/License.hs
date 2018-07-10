{-# LANGUAGE DeriveFunctor #-}
module Hpack.License where

import qualified Distribution.License as Cabal
import qualified Distribution.SPDX.License as SPDX
import           Distribution.Parsec.Class (eitherParsec)

data License a = DontTouch | CanSPDX a | MustSPDX a
  deriving (Eq, Show, Functor)

parseLicense :: String -> License SPDX.License
parseLicense license = case spdxLicense of
  Right l | isUnknown -> MustSPDX l
  Left  _ | isUnknown -> DontTouch
  Right l -> CanSPDX l
  Left  _ -> case cabalLicense of
    Right l -> CanSPDX (Cabal.licenseToSPDX l)
    Left _ -> DontTouch
  where
    spdxLicense :: Either String SPDX.License
    spdxLicense  = eitherParsec license

    cabalLicense :: Either String Cabal.License
    cabalLicense = eitherParsec license

    isUnknown = license `notElem` knownLicenses

    knownLicenses = [
        "GPL"
      , "GPL-2"
      , "GPL-3"
      , "LGPL"
      , "LGPL-2.1"
      , "LGPL-3"
      , "AGPL"
      , "AGPL-3"
      , "BSD2"
      , "BSD3"
      , "BSD4"
      , "MIT"
      , "ISC"
      , "MPL-2.0"
      , "Apache"
      , "Apache-2.0"
      , "PublicDomain"
      , "AllRightsReserved"
      , "OtherLicense"
      ]
