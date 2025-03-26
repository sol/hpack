{-# LANGUAGE OverloadedStrings #-}
module SpecHook where

import           Test.Hspec
import qualified VCR

hook :: Spec -> Spec
hook = aroundAll_ (VCR.with "test/fixtures/vcr-tape.yaml")
