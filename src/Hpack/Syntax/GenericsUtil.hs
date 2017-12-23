{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
module Hpack.Syntax.GenericsUtil (
  HasTypeName
, typeName
, Selectors
, selectors
) where

import           Data.Proxy
import           GHC.Generics

type HasTypeName a d m = (Datatype d, Generic a, Rep a ~ M1 D d m)

typeName :: forall a d m. (Datatype d, Generic a, Rep a ~ M1 D d m) => Proxy a -> String
typeName _ = datatypeName (undefined :: M1 D d x y)

selectors :: (Selectors (Rep a)) => Proxy a -> [String]
selectors = f
  where
    f :: forall a. (Selectors (Rep a)) => Proxy a -> [String]
    f _ = selNames (Proxy :: Proxy (Rep a))

class Selectors a where
  selNames :: Proxy a -> [String]

instance Selectors f => Selectors (M1 D x f) where
  selNames _ = selNames (Proxy :: Proxy f)

instance Selectors f => Selectors (M1 C x f) where
  selNames _ = selNames (Proxy :: Proxy f)

instance Selector s => Selectors (M1 S s (K1 R t)) where
  selNames _ = [selName (undefined :: M1 S s (K1 R t) ())]

instance (Selectors a, Selectors b) => Selectors (a :*: b) where
  selNames _ = selNames (Proxy :: Proxy a) ++ selNames (Proxy :: Proxy b)

instance Selectors U1 where
  selNames _ = []
