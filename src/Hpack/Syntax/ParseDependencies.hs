{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Hpack.Syntax.ParseDependencies where

import           Imports

import           Data.Aeson.Config.FromValue
import qualified Data.Aeson.Config.Key as Key

data Parse k v = Parse {
  parseString  :: Text -> Parser (k, v)
, parseListItem :: Object -> Parser v
, parseDictItem :: Value -> Parser v
, parseName :: Text -> k
}

parseDependencies :: Parse k v -> Value -> Parser [(k, v)]
parseDependencies parse@Parse{..} v = case v of
  String s -> return <$> parseString s
  Array xs -> parseArray (buildToolFromValue parse) xs
  Object o -> map (first (parseName . Key.toText)) <$> traverseObject parseDictItem o
  _ -> typeMismatch "Array, Object, or String" v

buildToolFromValue :: Parse k v -> Value -> Parser (k, v)
buildToolFromValue Parse{..} v = case v of
  String s -> parseString s
  Object o -> sourceDependency o
  _ -> typeMismatch "Object or String" v
  where
    sourceDependency o = (,) <$> (parseName <$> name) <*> parseListItem o
      where
        name :: Parser Text
        name = o .: "name"
