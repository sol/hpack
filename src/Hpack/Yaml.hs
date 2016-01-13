{-# LANGUAGE RecordWildCards #-}
module Hpack.Yaml (decodeYaml, decodeYamlBS) where

import qualified Data.ByteString as BS
import           Data.Yaml

decodeYaml :: FromJSON a => FilePath -> IO (Either String a)
decodeYaml file = do
  result <- decodeFileEither file
  return $ either (Left . errToString file) Right result

decodeYamlBS :: FromJSON a => BS.ByteString -> Either String a
decodeYamlBS bs = either (Left . errToString "-") Right (decodeEither' bs)

errToString :: FilePath -> ParseException -> String
errToString file err = file ++ case err of
  AesonException e -> ": " ++ e
  InvalidYaml (Just (YamlException s)) -> ": " ++ s
  InvalidYaml (Just (YamlParseException{..})) -> ":" ++ show yamlLine ++ ":" ++ show yamlColumn ++ ": " ++ yamlProblem ++ " " ++ yamlContext
    where YamlMark{..} = yamlProblemMark
  _ -> ": " ++ show err
