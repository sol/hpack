{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
module Hpack.Render.Hints (
  FormattingHints (..)
, sniffFormattingHints
, RenderSettings (..)
, defaultRenderSettings
, formattingHintsRenderSettings
#ifdef TEST
, extractFieldOrder
, extractSectionsFieldOrder
, sanitize
, unindent
, sniffAlignment
, splitField
, sniffIndentation
, sniffCommaStyle
#endif
) where

import           Imports

import           Data.Char
import           Data.Maybe

import           Hpack.Render.Dsl (Alignment(..), CommaStyle(..))
import           Hpack.Util

data FormattingHints = FormattingHints {
  formattingHintsFieldOrder :: [String]
, formattingHintsSectionsFieldOrder :: [(String, [String])]
, formattingHintsAlignment :: Maybe Alignment
, formattingHintsIndentation :: Maybe Int
, formattingHintsCommaStyle :: Maybe CommaStyle
} deriving (Eq, Show)

sniffFormattingHints :: [String] -> FormattingHints
sniffFormattingHints (sanitize -> input) = FormattingHints {
  formattingHintsFieldOrder = extractFieldOrder input
, formattingHintsSectionsFieldOrder = extractSectionsFieldOrder input
, formattingHintsAlignment = sniffAlignment input
, formattingHintsIndentation = sniffIndentation input
, formattingHintsCommaStyle = sniffCommaStyle input
}

sanitize :: [String] -> [String]
sanitize = filter (not . isPrefixOf "cabal-version:") . filter (not . null) . map stripEnd

stripEnd :: String -> String
stripEnd = reverse . dropWhile isSpace . reverse

extractFieldOrder :: [String] -> [String]
extractFieldOrder = map fst . catMaybes . map splitField

extractSectionsFieldOrder :: [String] -> [(String, [String])]
extractSectionsFieldOrder = map (fmap extractFieldOrder) . splitSections
  where
    splitSections input = case break startsWithSpace input of
      ([], []) -> []
      (xs, ys) -> case span startsWithSpace ys of
        (fields, zs) -> case reverse xs of
          name : _ -> (name, unindent fields) : splitSections zs
          _ -> splitSections zs

    startsWithSpace :: String -> Bool
    startsWithSpace xs = case xs of
      y : _ -> isSpace y
      _ -> False

unindent :: [String] -> [String]
unindent input = map (drop indentation) input
  where
    indentation = minimum $ map (length . takeWhile isSpace) input

data Indentation = Indentation {
  indentationFieldNameLength :: Int
, indentationPadding :: Int
}

indentationTotal :: Indentation -> Int
indentationTotal (Indentation fieldName padding) = fieldName + padding

sniffAlignment :: [String] -> Maybe Alignment
sniffAlignment input = case indentations of
  [] -> Nothing
  _ | all (indentationPadding >>> (== 1)) indentations -> Just 0
  _ -> case nub (map indentationTotal indentations) of
    [n] -> Just (Alignment n)
    _ -> Nothing
  where
    indentations :: [Indentation]
    indentations = catMaybes . map (splitField >=> indentation) $ input

    indentation :: (String, String) -> Maybe Indentation
    indentation (name, value) = case span isSpace value of
      (_, "") -> Nothing
      (padding, _) -> Just Indentation {
        indentationFieldNameLength = succ $ length name
      , indentationPadding = length padding
      }

splitField :: String -> Maybe (String, String)
splitField field = case span isNameChar field of
  (xs, ':':ys) -> Just (xs, ys)
  _ -> Nothing
  where
    isNameChar = (`elem` nameChars)
    nameChars = ['a'..'z'] ++ ['A'..'Z'] ++ "-"

sniffIndentation :: [String] -> Maybe Int
sniffIndentation input = sniffFrom "library" <|> sniffFrom "executable"
  where
    sniffFrom :: String -> Maybe Int
    sniffFrom section = case findSection . removeEmptyLines $ input of
      _ : x : _ -> Just . length $ takeWhile isSpace x
      _ -> Nothing
      where
        findSection = dropWhile (not . isPrefixOf section)

    removeEmptyLines :: [String] -> [String]
    removeEmptyLines = filter $ any (not . isSpace)

sniffCommaStyle :: [String] -> Maybe CommaStyle
sniffCommaStyle input
  | any startsWithComma input = Just LeadingCommas
  | any (startsWithComma . reverse) input = Just TrailingCommas
  | otherwise = Nothing
  where
    startsWithComma = isPrefixOf "," . dropWhile isSpace

data RenderSettings = RenderSettings {
  renderSettingsIndentation :: Int
, renderSettingsFieldAlignment :: Alignment
, renderSettingsCommaStyle :: CommaStyle
} deriving (Eq, Show)

defaultRenderSettings :: RenderSettings
defaultRenderSettings = RenderSettings 2 0 LeadingCommas

formattingHintsRenderSettings :: FormattingHints -> RenderSettings
formattingHintsRenderSettings FormattingHints{..} = defaultRenderSettings {
  renderSettingsIndentation = indentation
, renderSettingsCommaStyle = commaStyle
} where
    indentation = max def $ fromMaybe def formattingHintsIndentation
      where def = renderSettingsIndentation defaultRenderSettings
    commaStyle = fromMaybe (renderSettingsCommaStyle defaultRenderSettings) formattingHintsCommaStyle
