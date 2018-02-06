{-# LANGUAGE RecordWildCards #-}
module Hpack.Run (
  RunOptions(..)
, defaultRunOptions
, run
, renderPackage
, RenderSettings(..)
, Alignment(..)
, CommaStyle(..)
, defaultRenderSettings
) where

import           Data.Maybe
import           System.Exit
import           System.FilePath
import           System.Directory
import qualified Data.Aeson as Aeson

import           Hpack.Util
import           Hpack.Render
import           Hpack.Yaml
import           Hpack.Config
import           Hpack.Render.Hints

data RunOptions = RunOptions {
  runOptionsConfigDir :: Maybe FilePath
, runOptionsConfigFile :: FilePath
, runOptionsDecode :: FilePath -> IO (Either String Aeson.Value)
}

defaultRunOptions :: RunOptions
defaultRunOptions = RunOptions Nothing packageConfig decodeYaml

run :: RunOptions -> IO ([String], FilePath, String)
run (RunOptions mDir c decode) = do
  let dir = fromMaybe "" mDir
  userDataDir <- getAppUserDataDirectory "hpack"
  mPackage <- readPackageConfigWith decode userDataDir (dir </> c)
  case mPackage of
    Right (pkg, warnings) -> do
      let cabalFile = dir </> (packageName pkg ++ ".cabal")

      old <- tryReadFile cabalFile

      let
        FormattingHints{..} = sniffFormattingHints (fromMaybe "" old)
        alignment = fromMaybe 16 formattingHintsAlignment
        settings = formattingHintsRenderSettings

        output = renderPackage settings alignment formattingHintsFieldOrder formattingHintsSectionsFieldOrder pkg

      return (warnings, cabalFile, output)
    Left err -> die err
