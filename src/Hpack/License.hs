{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
module Hpack.License where

import           Control.Arrow ((&&&))

import           Distribution.Pretty (prettyShow)
import           Distribution.Version (mkVersion)
import qualified Distribution.License as Cabal
import qualified Distribution.SPDX.License as SPDX
import           Distribution.Parsec (eitherParsec)

import qualified Data.License.Infer as Infer

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

inferLicense :: String -> Maybe (License SPDX.License)
inferLicense = fmap (uncurry CanSPDX . (id &&& Cabal.licenseToSPDX) . toLicense) . Infer.inferLicense
  where
    toLicense = \ case
      Infer.MIT -> Cabal.MIT
      Infer.ISC -> Cabal.ISC
      Infer.BSD2 -> Cabal.BSD2
      Infer.BSD3 -> Cabal.BSD3
      Infer.BSD4 -> Cabal.BSD4
      Infer.Apache_2_0 -> Cabal.Apache (Just $ mkVersion [2,0])
      Infer.MPL_2_0 -> Cabal.MPL (mkVersion [2,0])
      Infer.GPLv2 -> Cabal.GPL (Just $ mkVersion [2])
      Infer.GPLv3 -> Cabal.GPL (Just $ mkVersion [3])
      Infer.LGPLv2_1 -> Cabal.LGPL (Just $ mkVersion [2,1])
      Infer.LGPLv3 -> Cabal.LGPL (Just $ mkVersion [3])
      Infer.AGPLv3 -> Cabal.AGPL (Just $ mkVersion [3])
