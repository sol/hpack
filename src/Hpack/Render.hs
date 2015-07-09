{-# LANGUAGE RecordWildCards #-}

module Hpack.Render where

import Prelude ()
import Prelude.Compat

import Control.Applicative
import Data.Char
import Data.List.Compat
import Data.String

data Value =
    Literal String
  | CommaSeparatedList [String]
  | LineSeparatedList [String]
  | WordList [String]

data Field = Field String Value

data Stanza = Stanza String [Field]

data Lines = SingleLine String | MultipleLines [String]
  deriving (Eq, Show)

data RenderSettings = RenderSettings {
  renderSettingsIndentation :: Int
} deriving (Eq, Show)

defaultRenderSettings :: RenderSettings
defaultRenderSettings = RenderSettings 2

class Render a where
  render :: RenderSettings -> Int -> a -> [String]

instance Render Stanza where
  render settings nesting (Stanza name fields) = name : renderFields fields
    where
      renderFields :: [Field] -> [String]
      renderFields = concatMap (render settings $ succ nesting)

instance Render Field where
  render settings nesting (Field name v) = case renderValue v of
    SingleLine "" -> []
    SingleLine x -> [indent settings nesting (name ++ ": " ++ x)]
    MultipleLines [] -> []
    MultipleLines xs -> (indent settings nesting name ++ ":") : map (indent settings $ succ nesting) xs

renderValue :: Value -> Lines
renderValue (Literal s) = SingleLine s
renderValue (WordList ws) = SingleLine $ unwords ws
renderValue (CommaSeparatedList xs) = MultipleLines $
  map render_ (zip (True : repeat False) xs)
  where
    render_ :: (Bool, String) -> String
    render_ (isFirst, x)
      | isFirst   = "  " ++ x
      | otherwise = ", " ++ x
renderValue (LineSeparatedList xs) = MultipleLines $ map ("  " ++) xs

instance IsString Value where
  fromString = Literal

indent :: RenderSettings -> Int -> String -> String
indent RenderSettings{..} nesting s = replicate (nesting * renderSettingsIndentation) ' ' ++ s

sniffIndentation :: String -> Maybe Int
sniffIndentation s = sniffFrom "library" <|> sniffFrom "executable"
  where
    sniffFrom :: String -> Maybe Int
    sniffFrom section = case findSection . removeEmptyLines $ lines s of
      _ : x : _ -> Just . length $ takeWhile isSpace x
      _ -> Nothing
      where
        findSection = dropWhile (not . isPrefixOf section)

    removeEmptyLines :: [String] -> [String]
    removeEmptyLines = filter $ any (not . isSpace)

sniffRenderSettings :: String -> RenderSettings
sniffRenderSettings s = maybe defaultRenderSettings RenderSettings indentation
  where
    indentation = sniffIndentation s
