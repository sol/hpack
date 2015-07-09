{-# LANGUAGE RecordWildCards #-}

module Hpack.Render where

import           Prelude ()
import           Prelude.Compat

import           Control.Applicative
import           Data.Char
import           Data.List.Compat
import           Data.Maybe
import           Data.String

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
, renderSettingsTrailingCommas :: Bool
} deriving (Eq, Show)

defaultRenderSettings :: RenderSettings
defaultRenderSettings = RenderSettings 2 False

class Render a where
  render :: RenderSettings -> Int -> a -> [String]

instance Render Stanza where
  render settings nesting (Stanza name fields) = name : renderFields fields
    where
      renderFields :: [Field] -> [String]
      renderFields = concatMap (render settings $ succ nesting)

instance Render Field where
  render settings nesting (Field name v) = case renderValue settings v of
    SingleLine "" -> []
    SingleLine x -> [indent settings nesting (name ++ ": " ++ x)]
    MultipleLines [] -> []
    MultipleLines xs -> (indent settings nesting name ++ ":") : map (indent settings $ succ nesting) xs

renderValue :: RenderSettings -> Value -> Lines
renderValue RenderSettings{..} v = case v of
  Literal s -> SingleLine s
  WordList ws -> SingleLine $ unwords ws
  LineSeparatedList xs -> MultipleLines $ map ("  " ++) xs
  CommaSeparatedList xs -> MultipleLines $ ys
    where
      ys | renderSettingsTrailingCommas = trailingCommas
         | otherwise = leadingCommas
      leadingCommas = map render_ (zip (True : repeat False) xs)
        where
          render_ :: (Bool, String) -> String
          render_ (isFirst, x)
            | isFirst   = "  " ++ x
            | otherwise = ", " ++ x
      trailingCommas = map render_ (reverse $ zip (True : repeat False) (reverse xs))
        where
          render_ :: (Bool, String) -> String
          render_ (isLast, x)
            | isLast   = x
            | otherwise = x ++ ","

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
sniffRenderSettings s = RenderSettings indentation trailingCommas
  where
    indentation = fromMaybe (renderSettingsIndentation defaultRenderSettings) (sniffIndentation s)
    trailingCommas = renderSettingsTrailingCommas defaultRenderSettings
