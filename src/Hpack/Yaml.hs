{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Hpack.Yaml (
-- | /__NOTE:__/ This module is exposed to allow integration of Hpack into
-- other tools.  It is not meant for general use by end users.  The following
-- caveats apply:
--
-- * The API is undocumented, consult the source instead.
--
-- * The exposed types and functions primarily serve Hpack's own needs, not
-- that of a public API.  Breaking changes can happen as Hpack evolves.
--
-- As an Hpack user you either want to use the @hpack@ executable or a build
-- tool that supports Hpack (e.g. @stack@ or @cabal2nix@).

  decodeYaml
, module Data.Aeson.Config.FromValue
) where

import           Imports

import           Data.Yaml hiding (decodeFile, decodeFileWithWarnings)
import           Data.Yaml.Include
import           Data.Yaml.Internal (Warning(..))
import           Data.Aeson.Config.FromValue
import           Data.Aeson.Config.Parser (fromAesonPath, formatPath)
import           Hpack.Error (HpackError (..))

formatWarning :: FilePath -> Warning -> String
formatWarning file = \ case
  DuplicateKey path -> file ++ ": Duplicate field " ++ formatPath (fromAesonPath path)

decodeYaml :: FilePath -> IO (Either HpackError ([String], Value))
decodeYaml file = do
  result <- decodeFileWithWarnings file
  return $ either (Left . toHpackError file) (Right . first (map $ formatWarning file)) result

toHpackError :: FilePath -> ParseException -> HpackError
toHpackError file (AesonException s) = HpackParseAesonException file s
toHpackError file (InvalidYaml (Just (YamlException s))) = HpackParseYamlException file s
toHpackError yamlFile (InvalidYaml (Just (YamlParseException{..}))) = HpackParseYamlParseException {..}
 where
  YamlMark{..} = yamlProblemMark
-- All other ParseException values are reduced to their show result
toHpackError file e = HpackParseOtherException file (show e)
