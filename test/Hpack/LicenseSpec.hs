{-# LANGUAGE QuasiQuotes #-}
module Hpack.LicenseSpec (spec) where

import           Helper
import           Data.String.Interpolate

import           Distribution.Pretty (prettyShow)

import           Hpack.License

cabalLicenses :: [(String, License String)]
cabalLicenses = [
    ("GPL",               CanSPDX "LicenseRef-GPL")
  , ("GPL-2",             CanSPDX "GPL-2.0-only")
  , ("GPL-3",             CanSPDX "GPL-3.0-only")

  , ("LGPL",              CanSPDX "LicenseRef-LGPL")
  , ("LGPL-2.1",          CanSPDX "LGPL-2.1-only")
  , ("LGPL-3",            CanSPDX "LGPL-3.0-only")

  , ("AGPL",              CanSPDX "LicenseRef-AGPL")
  , ("AGPL-3",            CanSPDX "AGPL-3.0-only")


  , ("BSD2",              CanSPDX "BSD-2-Clause")
  , ("BSD3",              CanSPDX "BSD-3-Clause")
  , ("BSD4",              CanSPDX "BSD-4-Clause")

  , ("MIT",               CanSPDX "MIT")
  , ("ISC",               CanSPDX "ISC")

  , ("MPL-2.0",           CanSPDX "MPL-2.0")

  , ("Apache",            CanSPDX "LicenseRef-Apache")
  , ("Apache-2.0",        CanSPDX "Apache-2.0")

  , ("PublicDomain",      CanSPDX "LicenseRef-PublicDomain")
  , ("OtherLicense",      CanSPDX "LicenseRef-OtherLicense")
  , ("AllRightsReserved", CanSPDX "NONE")
  ]

spdxLicenses :: [(String, License String)]
spdxLicenses = [
    ("GPL-2.0-or-later",  MustSPDX "GPL-2.0-or-later")
  ]

unknownLicenses :: [(String, License String)]
unknownLicenses = [
    ("some-license",      DontTouch)
  ]

spec :: Spec
spec = do
  describe "parseLicense" $ do
    forM_ (cabalLicenses ++ spdxLicenses ++ unknownLicenses) $ \ (license, expected) -> do
      it [i|parses #{license}|] $ do
        prettyShow <$> parseLicense license `shouldBe` expected
