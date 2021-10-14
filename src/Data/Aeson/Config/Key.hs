{-# LANGUAGE CPP #-}
module Data.Aeson.Config.Key (module Data.Aeson.Config.Key) where

#if MIN_VERSION_aeson(2,0,0)

import           Data.Aeson.Key as Data.Aeson.Config.Key

#else

import           Data.Text (Text)
import qualified Data.Text as T

type Key = Text

toText :: Key -> Text
toText = id

toString :: Key -> String
toString = T.unpack

fromString :: String -> Key
fromString = T.pack

#endif
