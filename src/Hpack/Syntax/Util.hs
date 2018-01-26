{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Hpack.Syntax.Util (
  Generic
, genericParseJSON
, hyphenize
, module Data.Aeson.Types
) where

import qualified Data.Aeson.Types as Aeson
import           Data.Aeson.Types hiding (genericParseJSON)
import           Data.Data
import           GHC.Generics

import           Hpack.Syntax.GenericsUtil

genericParseJSON :: forall a d m. (GFromJSON Zero (Rep a), HasTypeName a d m) => Value -> Parser a
genericParseJSON = Aeson.genericParseJSON defaultOptions {fieldLabelModifier = hyphenize name}
  where
    name :: String
    name = typeName (Proxy :: Proxy a)

hyphenize :: String -> String -> String
hyphenize name =
  camelTo2 '-' . drop (length (dropWhile (== '_') $ reverse name)) . dropWhile (== '_')
