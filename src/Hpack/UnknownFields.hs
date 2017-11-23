{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Hpack.UnknownFields (
  FieldName
, HasFieldNames(..)
, hyphenize

, CaptureUnknownFields
, Preposition(..)
, formatUnknownFields
) where

import           Prelude ()
import           Prelude.Compat

import           Control.Monad.Compat
import           Data.Aeson.Types
import           Data.Data
import qualified Data.HashMap.Lazy as HashMap
import           Data.List.Compat
import qualified Data.Text as T
import           GHC.Generics

import           Hpack.GenericsUtil

newtype FieldName = FieldName {unFieldName :: String}

class HasFieldNames a where
  fieldNames :: Proxy a -> [FieldName]

  default fieldNames :: (HasTypeName a d m, Selectors (Rep a)) => Proxy a -> [FieldName]
  fieldNames proxy = map (FieldName . hyphenize (typeName proxy)) (selectors proxy)

  ignoreUnderscoredUnknownFields :: Proxy a -> Bool
  ignoreUnderscoredUnknownFields _ = False

hyphenize :: String -> String -> String
hyphenize name =
#if MIN_VERSION_aeson(0,10,0)
  camelTo2
#else
  camelTo
#endif
  '-' . drop (length name) . dropWhile (== '_')

data CaptureUnknownFields a = CaptureUnknownFields [FieldName] a
  deriving Functor

instance Applicative CaptureUnknownFields where
  pure = return
  (<*>) = ap

instance Monad CaptureUnknownFields where
  return = CaptureUnknownFields mempty
  (CaptureUnknownFields xs x) >>= f = CaptureUnknownFields (xs `mappend` ys) y
    where
      CaptureUnknownFields ys y = f x

captureUnknownFields :: forall a. (HasFieldNames a, FromJSON a) => Value -> Parser (CaptureUnknownFields a)
captureUnknownFields v = CaptureUnknownFields unknown <$> parseJSON v
  where
    unknown = getUnknownFields v (Proxy :: Proxy a)

instance (HasFieldNames a, FromJSON a) => FromJSON (CaptureUnknownFields a) where
  parseJSON = captureUnknownFields

getUnknownFields :: forall a. HasFieldNames a => Value -> Proxy a -> [FieldName]
getUnknownFields v _ = case v of
  Object o -> map FieldName (ignoreUnderscored unknown)
    where
      unknown = keys \\ fields
      keys = map T.unpack (HashMap.keys o)
      fields = map unFieldName $ fieldNames (Proxy :: Proxy a)
      ignoreUnderscored
        | ignoreUnderscoredUnknownFields (Proxy :: Proxy a) = filter (not . isPrefixOf "_")
        | otherwise = id
  _ -> []

data Preposition = In | For

formatUnknownFields :: Preposition -> String -> CaptureUnknownFields a -> ([String], a)
formatUnknownFields p name (CaptureUnknownFields unknownFields a) = (formatUnknownFields_ preposition name unknownFields, a)
  where
    preposition = case p of
      In -> "in"
      For -> "for"

formatUnknownFields_ :: String -> String -> [FieldName] -> [String]
formatUnknownFields_ preposition name = map f
  where
    f (FieldName field) = "Ignoring unknown field " ++ show field ++ " " ++ preposition ++ " " ++ name
