{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
module Hpack.Field.Options (
  Options(..)
, escapeOptionToken
, GhcOptions
, GhcProfOptions
, GhcjsOptions
, CppOptions
, CcOptions
, LdOptions
) where

import           Data.Aeson.Types hiding (Options(..))
import qualified Data.Text as T
import qualified Data.HashMap.Lazy as HM

import           Hpack.Util

newtype Options a = Options {
  unOptions :: List a
} deriving (Eq, Show, Functor, Foldable, Traversable)

instance FromJSON (Options String) where
  parseJSON v = Options <$> case v of
    Object hm -> parseHashMap hm
    _ -> parseJSON v
    where
      parseHashMap hm = List <$>
        foldMWithKey (\t k x -> maybe t (:t) <$> translateOptionMap k x) [] hm

      foldMWithKey f z0 t =
        let f' k x c z = f z k x >>= c
        in HM.foldrWithKey f' return t z0

      translateOptionMap key val = do
        let key' = T.unpack key

        let disableOption = Nothing
        let enableOption = Just key'
        let paramOption param = Just $ escapeOptionToken $ key' ++ "=" ++ param

        case val of
          Null       -> pure enableOption
          Bool True  -> pure enableOption
          Bool False -> pure disableOption
          String txt -> pure $ paramOption $ T.unpack txt
          Number n   -> pure $ paramOption $ show n
          _          -> typeMismatch "a flag or parameters" val

-- options fields are parsed as Haskell String Literal
escapeOptionToken :: String -> String
escapeOptionToken = show

type GhcOptions     = Options GhcOption
type GhcProfOptions = Options GhcProfOption
type GhcjsOptions   = Options GhcjsOption
type CppOptions     = Options CppOption
type CcOptions      = Options CcOption
type LdOptions      = Options LdOption
