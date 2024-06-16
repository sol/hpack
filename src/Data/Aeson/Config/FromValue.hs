{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveFunctor #-}
module Data.Aeson.Config.FromValue (
  FromValue(..)
, Parser
, Result
, decodeValue

, Generic
, GenericDecode
, genericFromValue
, Options(..)
, genericFromValueWith

, typeMismatch
, withObject
, withText
, withString
, withArray
, withNumber
, withBool

, parseArray
, traverseObject

, (.:)
, (.:?)

, Key
, Value(..)
, Object
, Array

, Alias(..)
, unAlias
) where

import           Imports

import           Data.Monoid (Last(..))
import           GHC.Generics
import           GHC.TypeLits
import           Data.Proxy

import           Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import qualified Data.Vector as V
import           Data.Aeson.Config.Key (Key)
import qualified Data.Aeson.Config.Key as Key
import           Data.Aeson.Config.KeyMap (member)
import qualified Data.Aeson.Config.KeyMap as KeyMap

import           Data.Aeson.Types (FromJSON(..))

import           Data.Aeson.Config.Util
import           Data.Aeson.Config.Parser

type Result a = Either String (a, [String], [(String, String)])

decodeValue :: FromValue a => Value -> Result a
decodeValue = runParser fromValue

(.:) :: FromValue a => Object -> Key -> Parser a
(.:) = explicitParseField fromValue

(.:?) :: FromValue a => Object -> Key -> Parser (Maybe a)
(.:?) = explicitParseFieldMaybe fromValue

class FromValue a where
  fromValue :: Value -> Parser a
  default fromValue :: forall d m. (Generic a, Rep a ~ D1 d m, Datatype d, GenericDecode (Rep a)) => Value -> Parser a
  fromValue = genericFromValue

genericFromValue :: forall a d m. (Generic a, Rep a ~ D1 d m, Datatype d, GenericDecode (Rep a)) => Value -> Parser a
genericFromValue = genericFromValueWith (Options $ hyphenize name)
  where
    name :: String
    name = datatypeName (undefined :: D1 d m p)

instance FromValue Bool where
  fromValue = liftParser . parseJSON

instance FromValue Int where
  fromValue = liftParser . parseJSON

instance FromValue Text where
  fromValue = liftParser . parseJSON

instance {-# OVERLAPPING #-} FromValue String where
  fromValue = liftParser . parseJSON

instance FromValue a => FromValue (Maybe a) where
  fromValue value = liftParser (parseJSON value) >>= traverse fromValue

instance FromValue a => FromValue [a] where
  fromValue = withArray (parseArray fromValue)

parseArray :: (Value -> Parser a) -> Array -> Parser [a]
parseArray f = zipWithM (parseIndexed f) [0..] . V.toList
  where
    parseIndexed :: (Value -> Parser a) -> Int -> Value -> Parser a
    parseIndexed p n value = p value <?> Index n

instance FromValue a => FromValue (Map String a) where
  fromValue = withObject $ \ o -> do
    xs <- traverseObject fromValue o
    return $ Map.fromList (map (first Key.toString) xs)

traverseObject :: (Value -> Parser a) -> Object -> Parser [(Key, a)]
traverseObject f o = do
  forM (KeyMap.toList o) $ \ (name, value) ->
    (,) name <$> f value <?> Key name

instance (FromValue a, FromValue b) => FromValue (a, b) where
  fromValue v = (,) <$> fromValue v <*> fromValue v

instance (FromValue a, FromValue b) => FromValue (Either a b) where
  fromValue v = Left <$> fromValue v <|> Right <$> fromValue v

data Options = Options {
  optionsRecordSelectorModifier :: String -> String
}

genericFromValueWith :: (Generic a, GenericDecode (Rep a)) => Options -> Value -> Parser a
genericFromValueWith opts = fmap to . genericDecode  opts

class GenericDecode f where
  genericDecode :: Options -> Value -> Parser (f p)

instance (GenericDecode a) => GenericDecode (D1 d a) where
  genericDecode opts = fmap M1 . genericDecode opts

instance (GenericDecode a) => GenericDecode (C1 c a) where
  genericDecode opts = fmap M1 . genericDecode opts

instance (GenericDecode a, GenericDecode b) => GenericDecode (a :*: b) where
  genericDecode opts o = (:*:) <$> genericDecode opts o <*> genericDecode opts o

type RecordField sel a = S1 sel (Rec0 a)

instance (Selector sel, FromValue a) => GenericDecode (RecordField sel a) where
  genericDecode = accessFieldWith (.:)

instance {-# OVERLAPPING #-} (Selector sel, FromValue a) => GenericDecode (RecordField sel (Maybe a)) where
  genericDecode = accessFieldWith (.:?)

instance {-# OVERLAPPING #-} (Selector sel, FromValue a) => GenericDecode (RecordField sel (Last a)) where
  genericDecode = accessFieldWith (\ value key -> Last <$> (value .:? key))

instance {-# OVERLAPPING #-} (Selector sel, FromValue a, KnownBool deprecated, KnownSymbol alias) => GenericDecode (RecordField sel (Alias deprecated alias (Maybe a))) where
  genericDecode = accessFieldWith (\ value key -> aliasAccess (.:?) value (Alias key))

instance {-# OVERLAPPING #-} (Selector sel, FromValue a, KnownBool deprecated, KnownSymbol alias) => GenericDecode (RecordField sel (Alias deprecated alias (Last a))) where
  genericDecode = accessFieldWith (\ value key -> fmap Last <$> aliasAccess (.:?) value (Alias key))

aliasAccess :: forall deprecated alias a. (KnownBool deprecated, KnownSymbol alias) => (Object -> Key -> Parser a) -> Object -> (Alias deprecated alias Key) -> Parser (Alias deprecated alias a)
aliasAccess op value (Alias key)
  | alias `member` value && not (key `member` value) = Alias <$> value `op` alias <* deprecated
  | otherwise = Alias <$> value `op` key
  where
    deprecated = case boolVal (Proxy @deprecated) of
      False -> return ()
      True -> markDeprecated alias key
    alias = Key.fromString (symbolVal $ Proxy @alias)

accessFieldWith :: forall sel a p. Selector sel => (Object -> Key -> Parser a) -> Options -> Value -> Parser (RecordField sel a p)
accessFieldWith op Options{..} v = M1 . K1 <$> withObject (`op` Key.fromString label) v
  where
    label = optionsRecordSelectorModifier $ selName (undefined :: RecordField sel a p)

newtype Alias (deprecated :: Bool) (alias :: Symbol) a = Alias a
  deriving (Show, Eq, Semigroup, Monoid, Functor)

unAlias :: Alias deprecated alias a -> a
unAlias (Alias a) = a

class KnownBool (a :: Bool) where
  boolVal :: Proxy a -> Bool

instance KnownBool 'True where
  boolVal _ = True

instance KnownBool 'False where
  boolVal _ = False
