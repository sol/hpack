{-# LANGUAGE RecordWildCards #-}
module Hpack.Yaml where

import           Data.Yaml hiding (decodeFile, decodeFileEither)
import           Data.Yaml.Include

decodeYaml :: FromJSON a => FilePath -> IO (Either String a)
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
