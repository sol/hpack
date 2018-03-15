{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Aeson.Config.Types where

import           Data.Semigroup (Semigroup(..))
import           Data.Bitraversable
import           Data.Bifoldable
import           Data.Bifunctor

import           Data.Aeson.Config.FromValue

newtype List a = List {fromList :: [a]}
  deriving (Eq, Show, Functor, Foldable, Traversable, Semigroup, Monoid)

instance FromValue a => FromValue (List a) where
  fromValue v = List <$> case v of
    Array _ -> fromValue v
    _ -> return <$> fromValue v

fromMaybeList :: Maybe (List a) -> [a]
fromMaybeList = maybe [] fromList

data Product a b = Product a b
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance (Semigroup a, Semigroup b, Monoid a, Monoid b) => Monoid (Product a b) where
  mempty = Product mempty mempty
  mappend = (<>)

instance (Semigroup a, Semigroup b) => Semigroup (Product a b) where
  Product a1 b1 <> Product a2 b2 = Product (a1 <> a2) (b1 <> b2)

instance Bifunctor Product where
  bimap fa fb (Product a b) = Product (fa a) (fb b)

instance Bifoldable Product where
  bifoldMap = bifoldMapDefault

instance Bitraversable Product where
  bitraverse fa fb (Product a b) = Product <$> fa a <*> fb b

instance (FromValue a, FromValue b) => FromValue (Product a b) where
  fromValue v = Product <$> fromValue v <*> fromValue v
