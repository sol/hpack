{-# LANGUAGE CPP #-}
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

#if MIN_VERSION_aeson(1,0,0)
genericParseJSON :: forall a d m. (GFromJSON Zero (Rep a), HasTypeName a d m) => Value -> Parser a
#else
genericParseJSON :: forall a d m. (GFromJSON (Rep a), HasTypeName a d m) => Value -> Parser a
#endif
genericParseJSON = Aeson.genericParseJSON defaultOptions {fieldLabelModifier = hyphenize name}
  where
    name :: String
    name = typeName (Proxy :: Proxy a)

hyphenize :: String -> String -> String
hyphenize name =
#if MIN_VERSION_aeson(0,10,0)
  camelTo2
#else
  camelTo
#endif
  '-' . drop (length name) . dropWhile (== '_')
