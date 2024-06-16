{-# LANGUAGE CPP #-}
module Data.Aeson.Config.KeyMap (module KeyMap) where

#if MIN_VERSION_aeson(2,0,0)
import           Data.Aeson.KeyMap as KeyMap
#else
import           Data.HashMap.Strict as KeyMap
#endif
