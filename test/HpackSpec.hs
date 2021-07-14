module HpackSpec (spec) where

import           Helper

import           Prelude hiding (readFile)
import qualified Prelude as Prelude

import           Control.DeepSeq

import           Hpack.Config
import           Hpack.CabalFile
import           Hpack hiding (hpack)

readFile :: FilePath -> IO String
readFile name = Prelude.readFile name >>= (return $!!)

spec :: Spec
spec = do
  describe "header" $ do
    it "generates header" $ do
      header "foo.yaml" Nothing Nothing `shouldBe` [
          "-- This file has been generated from foo.yaml by hpack."
        , "--"
        , "-- see: https://github.com/sol/hpack"
        , ""
        ]

    context "with hpack version" $ do
      it "includes hpack version" $ do
        header "foo.yaml" (Just $ makeVersion [0,34,0]) Nothing `shouldBe` [
            "-- This file has been generated from foo.yaml by hpack version 0.34.0."
          , "--"
          , "-- see: https://github.com/sol/hpack"
          , ""
          ]

    context "with hash" $ do
      it "includes hash" $ do
        header "foo.yaml" Nothing (Just "some-hash") `shouldBe` [
            "-- This file has been generated from foo.yaml by hpack."
          , "--"
          , "-- see: https://github.com/sol/hpack"
          , "--"
          , "-- hash: some-hash"
          , ""
          ]

  describe "renderCabalFile" $ do
    it "is inverse to readCabalFile" $ do
      expected <- lines <$> readFile "hpack.cabal"
      Just c <- readCabalFile "hpack.cabal"
      renderCabalFile "package.yaml" c `shouldBe` expected

  describe "hpackResult" $ around_ inTempDirectory $ before_ (writeFile packageConfig "name: foo") $ do
    let
      file = "foo.cabal"

      hpackWithVersion v = hpackResultWithVersion (makeVersion v) defaultOptions
      hpackWithStrategy strategy = hpackResult defaultOptions { optionsGenerateHashStrategy = strategy }
      hpackForce = hpackResult defaultOptions {optionsForce = Force}

      generated = Result [] file Generated
      modifiedManually = Result [] file ExistingCabalFileWasModifiedManually
      outputUnchanged = Result [] file OutputUnchanged
      alreadyGeneratedByNewerHpack = Result [] file AlreadyGeneratedByNewerHpack

      modifyPackageConfig = writeFile packageConfig $ unlines [
          "name: foo"
        , "version: 0.1.0"
        ]

      modifyCabalFile = do
        xs <- readFile file
        writeFile file $ xs ++ "foo\n"

      manuallyCreateCabalFile = do
        writeFile file "some existing cabal file"

      doesNotGenerateHash :: HasCallStack => GenerateHashStrategy -> Spec
      doesNotGenerateHash strategy = do
        it "does not generate hash" $ do
          hpackWithStrategy strategy `shouldReturn` generated
          readFile file >>= (`shouldNotContain` "hash")

      generatesHash :: HasCallStack => GenerateHashStrategy -> Spec
      generatesHash strategy = do
        it "generates hash" $ do
          hpackWithStrategy strategy `shouldReturn` generated
          readFile file >>= (`shouldContain` "hash")

      doesNotOverwrite :: HasCallStack => GenerateHashStrategy -> Spec
      doesNotOverwrite strategy = do
        it "does not overwrite cabal file" $ do
          existing <- readFile file
          hpackWithStrategy strategy `shouldReturn` modifiedManually
          readFile file `shouldReturn` existing

      with strategy item = context ("with " ++ show strategy) $ item strategy

    context "without an existing cabal file" $ do
      with ForceHash generatesHash
      with ForceNoHash doesNotGenerateHash
      with PreferNoHash doesNotGenerateHash

    context "with an existing cabal file" $ do
      context "without a hash" $ before_ (hpackWithStrategy ForceNoHash >> modifyPackageConfig) $ do
        with ForceHash generatesHash
        with ForceNoHash doesNotGenerateHash
        with PreferNoHash doesNotGenerateHash

      context "with a hash" $ before_ (hpackWithStrategy ForceHash >> modifyPackageConfig) $ do
        with ForceHash generatesHash
        with ForceNoHash doesNotGenerateHash
        with PreferNoHash generatesHash

        context "with manual modifications" $ before_ modifyCabalFile $ do
          with ForceHash doesNotOverwrite
          with ForceNoHash doesNotGenerateHash
          with PreferNoHash doesNotOverwrite

      context "when created manually" $ before_ manuallyCreateCabalFile $ do
        with ForceHash doesNotOverwrite
        with ForceNoHash doesNotOverwrite
        with PreferNoHash doesNotOverwrite

        context "with --force" $ do
          it "overwrites cabal file" $ do
            hpackForce `shouldReturn` generated

      context "when generated with a newer version of hpack" $ do
        it "does not overwrite cabal file" $ do
          _ <- hpackWithVersion [0,22,0]
          old <- readFile file
          modifyPackageConfig
          hpackWithVersion [0,20,0] `shouldReturn` alreadyGeneratedByNewerHpack
          readFile file `shouldReturn` old

      context "when only the hpack version in the cabal file header changed" $ do
        it "does not overwrite cabal file" $ do
          _ <- hpackWithVersion [0,22,0]
          old <- readFile file
          hpackWithVersion [0,30,0] `shouldReturn` outputUnchanged
          readFile file `shouldReturn` old

        it "does not complain if it's newer" $ do
          _ <- hpackWithVersion [0,22,0]
          old <- readFile file
          hpackWithVersion [0,20,0] `shouldReturn` outputUnchanged
          readFile file `shouldReturn` old
