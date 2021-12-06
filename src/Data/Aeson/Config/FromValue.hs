{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

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
) where

import           Imports

import           GHC.Generics

import           Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import qualified Data.Vector as V
import           Data.Aeson.Config.Key (Key)
import qualified Data.Aeson.Config.Key as Key
import qualified Data.Aeson.Config.KeyMap as KeyMap

import           Data.Aeson.Types (FromJSON(..))

import           Data.Aeson.Config.Util
import           Data.Aeson.Config.Parser

type Result a = Either String (a, [String])

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

instance (Selector sel, FromValue a) => GenericDecode (S1 sel (Rec0 a)) where
  genericDecode = accessFieldWith (.:)

instance {-# OVERLAPPING #-} (Selector sel, FromValue a) => GenericDecode (S1 sel (Rec0 (Maybe a))) where
  genericDecode = accessFieldWith (.:?)

accessFieldWith :: forall sel a p. Selector sel => (Object -> Key -> Parser a) -> Options -> Value -> Parser (S1 sel (Rec0 a) p)
accessFieldWith op Options{..} v = M1 . K1 <$> withObject (`op` Key.fromString label) v
  where
    label = optionsRecordSelectorModifier $ selName (undefined :: S1 sel (Rec0 a) p)
