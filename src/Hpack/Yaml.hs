{-# LANGUAGE RecordWildCards #-}
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
) where

import           Data.Yaml hiding (decodeFile, decodeFileEither)
import           Data.Yaml.Include

decodeYaml :: FilePath -> IO (Either String Value)
decodeYaml file = do
  result <- decodeFileEither file
  return $ either (Left . errToString) Right result
  where
    errToString err = file ++ case err of
      AesonException e -> ": " ++ e
      InvalidYaml (Just (YamlException s)) -> ": " ++ s
      InvalidYaml (Just (YamlParseException{..})) -> ":" ++ show yamlLine ++ ":" ++ show yamlColumn ++ ": " ++ yamlProblem ++ " " ++ yamlContext
        where YamlMark{..} = yamlProblemMark
      _ -> ": " ++ show err
