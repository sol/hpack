{-# LANGUAGE CPP #-}
module Hpack.LicenseSpec (spec) where

import           Helper
import           Data.Maybe
import           Data.String.Interpolate

import           Distribution.Pretty (prettyShow)
import           Distribution.Parsec (simpleParsec)
import qualified Distribution.License as Cabal

import           Hpack.License

cabal :: String -> Cabal.License
cabal = fromJust . simpleParsec

cabalLicenses :: [(String, License String)]
cabalLicenses = [
    ("GPL",               CanSPDX (cabal "GPL") "LicenseRef-GPL")
  , ("GPL-2",             CanSPDX (cabal "GPL-2") "GPL-2.0-only")
  , ("GPL-3",             CanSPDX (cabal "GPL-3") "GPL-3.0-only")

  , ("LGPL",              CanSPDX (cabal "LGPL") "LicenseRef-LGPL")
  , ("LGPL-2.1",          CanSPDX (cabal "LGPL-2.1") "LGPL-2.1-only")
  , ("LGPL-3",            CanSPDX (cabal "LGPL-3") "LGPL-3.0-only")

  , ("AGPL",              CanSPDX (cabal "AGPL") "LicenseRef-AGPL")
  , ("AGPL-3",            CanSPDX (cabal "AGPL-3") "AGPL-3.0-only")


  , ("BSD2",              CanSPDX (cabal "BSD2") "BSD-2-Clause")
  , ("BSD3",              CanSPDX (cabal "BSD3") "BSD-3-Clause")
  , ("BSD4",              CanSPDX (cabal "BSD4") "BSD-4-Clause")

  , ("MIT",               CanSPDX (cabal "MIT") "MIT")
  , ("ISC",               CanSPDX (cabal "ISC") "ISC")

  , ("MPL-2.0",           CanSPDX (cabal "MPL-2.0") "MPL-2.0")

  , ("Apache",            CanSPDX (cabal "Apache") "LicenseRef-Apache")
  , ("Apache-2.0",        CanSPDX (cabal "Apache-2.0") "Apache-2.0")

  , ("PublicDomain",      CanSPDX (cabal "PublicDomain") "LicenseRef-PublicDomain")
  , ("OtherLicense",      CanSPDX (cabal "OtherLicense") "LicenseRef-OtherLicense")
  , ("AllRightsReserved", CanSPDX (cabal "AllRightsReserved") "NONE")
  ]

spdxLicenses :: [(String, License String)]
spdxLicenses = [
    ("GPL-2.0-or-later",  MustSPDX "GPL-2.0-or-later")
  ]

unknownLicenses :: [(String, License String)]
unknownLicenses = [
    ("some-license",      DontTouch "some-license")
  ]

spec :: Spec
spec = do
  describe "parseLicense" $ do
    forM_ (cabalLicenses ++ spdxLicenses ++ unknownLicenses) $ \ (license, expected) -> do
      it [i|parses #{license}|] $ do
        prettyShow <$> parseLicense license `shouldBe` expected
