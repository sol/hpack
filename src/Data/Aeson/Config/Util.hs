module Data.Aeson.Config.Util where

import           Data.Aeson.Types (camelTo2)

hyphenize :: String -> String -> String
hyphenize name = camelTo2 '-' . dropPrefix . dropWhile (== '_')
  where
    dropPrefix = drop (length (dropWhile (== '_') $ reverse name))
