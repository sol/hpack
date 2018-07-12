{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ViewPatterns #-}
module Hpack.License where

import           Control.Arrow ((&&&))
import           Data.List
import           Data.Ord (comparing)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Metrics

import           Distribution.Pretty (prettyShow)
import qualified Distribution.License as Cabal
import qualified Distribution.SPDX.License as SPDX
import           Distribution.SPDX.LicenseId
import           Distribution.Parsec.Class (eitherParsec)

import           Hpack.SpdxLicenses (licenses)

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

probabilities :: Text -> [(LicenseId, Double)]
probabilities license = map (fmap probability) licenses
  where
    probability = realToFrac . levenshteinNorm license

inferLicense :: String -> Maybe (License String)
inferLicense (T.pack -> xs) = case maximumBy (comparing snd) (probabilities xs) of
  (license, n) | n > 0.85 -> Just (toLicense license)
  _ -> Nothing
  where
    toLicense :: LicenseId -> License String
    toLicense license = (case license of
      MIT -> CanSPDX Cabal.MIT
      BSD_2_Clause -> CanSPDX Cabal.BSD2
      BSD_3_Clause -> CanSPDX Cabal.BSD3
      BSD_4_Clause -> CanSPDX Cabal.BSD4
      _ -> MustSPDX
      ) (licenseId license)
