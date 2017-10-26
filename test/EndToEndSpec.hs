{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ConstraintKinds #-}
module EndToEndSpec (spec) where

import           Helper

import           System.Exit.Compat
import           Data.Maybe
import           Data.String.Interpolate
import           Data.String.Interpolate.Util

import           Hpack.Run (renderPackage)
import           Hpack.Config (packageConfig, readPackageConfig)
import           Hpack.FormattingHints (FormattingHints(..), sniffFormattingHints)

spec :: Spec
spec = around_ (inTempDirectoryNamed "foo") $ do
  describe "hpack" $ do
    context "with executable section" $ do
      it "accepts arbitrary entry points as main" $ do
        [i|
        executables:
          foo:
            main: Foo
        |] `shouldRenderTo` Executable "foo" [i|
        main-is: Foo.hs
        ghc-options: -main-is Foo
        |]

run :: FilePath -> String -> IO ([String], String)
run c old = do
  mPackage <- readPackageConfig c
  case mPackage of
    Right (warnings, pkg) -> do
      let
        FormattingHints{..} = sniffFormattingHints old
        alignment = fromMaybe 0 formattingHintsAlignment
        settings = formattingHintsRenderSettings
        output = renderPackage settings alignment formattingHintsFieldOrder formattingHintsSectionsFieldOrder pkg
      return (warnings, output)
    Left err -> die err

newtype PlainString = PlainString String
  deriving Eq

instance Show PlainString where
  show (PlainString xs) = xs

shouldRenderTo :: HasCallStack => String -> Result -> Expectation
shouldRenderTo input r = do
  writeFile packageConfig input
  (_ , output) <- run packageConfig expected
  PlainString (dropEmptyLines output) `shouldBe` PlainString expected
  where
    expected = dropEmptyLines (renderResult "foo" r)
    dropEmptyLines = unlines . filter (not . null) . lines

data Result =
    Library String
  | Executable String String

renderResult :: String -> Result -> String
renderResult name = \ case
  Library e -> unindent [i|
name: #{name}
version: 0.0.0
build-type: Simple
cabal-version: >= 1.10

library
#{indentBy 2 $ unindent e}
  default-language: Haskell2010
|]
  Executable executableName e -> unindent [i|
name: #{name}
version: 0.0.0
build-type: Simple
cabal-version: >= 1.10

executable #{executableName}
#{indentBy 2 $ unindent e}
  default-language: Haskell2010
|]

indentBy :: Int -> String -> String
indentBy n = unlines . map (replicate n ' ' ++) . lines
