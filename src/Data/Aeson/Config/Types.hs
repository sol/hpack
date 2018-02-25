{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Aeson.Config.Types where

import           Data.Monoid hiding (Product)
import           Data.Bitraversable
import           Data.Bifoldable
import           Data.Bifunctor

import           Data.Aeson.Config.FromValue

newtype List a = List {fromList :: [a]}
#if !MIN_VERSION_base(4,11,0)
  deriving (Eq, Show, Functor, Foldable, Traversable, Monoid)
#else
  deriving (Eq, Show, Functor, Foldable, Traversable, Semigroup, Monoid)
#endif

instance FromValue a => FromValue (List a) where
  fromValue v = List <$> case v of
    Array _ -> fromValue v
    _ -> return <$> fromValue v

fromMaybeList :: Maybe (List a) -> [a]
fromMaybeList = maybe [] fromList

data Product a b = Product a b
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance (Monoid a, Monoid b) => Monoid (Product a b) where
  mempty = Product mempty mempty
#if !MIN_VERSION_base(4,11,0)
  Product a1 b1 `mappend` Product a2 b2 = Product (a1 <> a2) (b1 <> b2)
#else
instance (Semigroup a, Semigroup b) => Semigroup (Product a b) where
  Product a1 b1 <>        Product a2 b2 = Product (a1 <> a2) (b1 <> b2)
#endif

instance Bifunctor Product where
  bimap fa fb (Product a b) = Product (fa a) (fb b)

instance Bifoldable Product where
  bifoldMap = bifoldMapDefault

instance Bitraversable Product where
  bitraverse fa fb (Product a b) = Product <$> fa a <*> fb b

instance (FromValue a, FromValue b) => FromValue (Product a b) where
  fromValue v = Product <$> fromValue v <*> fromValue v
