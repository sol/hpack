{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
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
  deriving (Eq, Show)

data Element = Stanza String [Element] | Field String Value
  deriving (Eq, Show)

data Lines = SingleLine String | MultipleLines [String]
  deriving (Eq, Show)

data CommaStyle = LeadingCommas | TrailingCommas
  deriving (Eq, Show)

data RenderSettings = RenderSettings {
  renderSettingsIndentation :: Int
, renderSettingsCommaStyle :: CommaStyle
} deriving (Eq, Show)

defaultRenderSettings :: RenderSettings
defaultRenderSettings = RenderSettings 2 LeadingCommas

render :: RenderSettings -> Int -> Element -> [String]
render settings nesting (Stanza name elements) = indent settings nesting name : renderElements elements
  where
    renderElements :: [Element] -> [String]
    renderElements = concatMap (render settings $ succ nesting)
render settings nesting (Field name v) = case renderValue settings v of
  SingleLine "" -> []
  SingleLine x -> [indent settings nesting (name ++ ": " ++ x)]
  MultipleLines [] -> []
  MultipleLines xs -> (indent settings nesting name ++ ":") : map (indent settings $ succ nesting) xs

renderValue :: RenderSettings -> Value -> Lines
renderValue RenderSettings{..} v = case v of
  Literal s -> SingleLine s
  WordList ws -> SingleLine $ unwords ws
  LineSeparatedList xs -> renderLineSeparatedList renderSettingsCommaStyle xs
  CommaSeparatedList xs -> renderCommaSeparatedList renderSettingsCommaStyle xs

renderLineSeparatedList :: CommaStyle -> [String] -> Lines
renderLineSeparatedList style = MultipleLines . map (padding ++)
  where
    padding = case style of
      LeadingCommas -> "  "
      TrailingCommas -> ""

renderCommaSeparatedList :: CommaStyle -> [String] -> Lines
renderCommaSeparatedList style = MultipleLines . case style of
  LeadingCommas -> map renderLeadingComma . zip (True : repeat False)
  TrailingCommas -> map renderTrailingComma . reverse . zip (True : repeat False) . reverse
  where
    renderLeadingComma :: (Bool, String) -> String
    renderLeadingComma (isFirst, x)
      | isFirst   = "  " ++ x
      | otherwise = ", " ++ x

    renderTrailingComma :: (Bool, String) -> String
    renderTrailingComma (isLast, x)
      | isLast    = x
      | otherwise = x ++ ","

instance IsString Value where
  fromString = Literal

indent :: RenderSettings -> Int -> String -> String
indent RenderSettings{..} nesting s = replicate (nesting * renderSettingsIndentation) ' ' ++ s

sniffIndentation :: String -> Maybe Int
sniffIndentation input = sniffFrom "library" <|> sniffFrom "executable"
  where
    sniffFrom :: String -> Maybe Int
    sniffFrom section = case findSection . removeEmptyLines $ lines input of
      _ : x : _ -> Just . length $ takeWhile isSpace x
      _ -> Nothing
      where
        findSection = dropWhile (not . isPrefixOf section)

    removeEmptyLines :: [String] -> [String]
    removeEmptyLines = filter $ any (not . isSpace)

sniffCommaStyle :: String -> Maybe CommaStyle
sniffCommaStyle (lines -> input)
  | any startsWithComma input = Just LeadingCommas
  | any (startsWithComma . reverse) input = Just TrailingCommas
  | otherwise = Nothing
  where
    startsWithComma = isPrefixOf "," . dropWhile isSpace

sniffRenderSettings :: String -> RenderSettings
sniffRenderSettings input = RenderSettings indentation trailingCommas
  where
    indentation = fromMaybe (renderSettingsIndentation defaultRenderSettings) (sniffIndentation input)
    trailingCommas = fromMaybe (renderSettingsCommaStyle defaultRenderSettings) (sniffCommaStyle input)
