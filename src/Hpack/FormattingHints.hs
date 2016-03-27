{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}
module Hpack.FormattingHints (
  FormattingHints (..)
, sniffFormattingHints
#ifdef TEST
, extractFieldOrderHint
, sniffAlignment
, splitField
, sniffIndentation
, sniffCommaStyle
#endif
) where

import           Data.Char
import           Data.Maybe
import           Data.List
import           Control.Applicative

import           Hpack.Render

data FormattingHints = FormattingHints {
  formattingHintsFieldOrder :: [String]
, formattingHintsAlignment :: Maybe Alignment
, formattingHintsRenderSettings :: RenderSettings
} deriving (Eq, Show)

sniffFormattingHints :: String -> FormattingHints
sniffFormattingHints input = FormattingHints {
  formattingHintsFieldOrder = extractFieldOrderHint input
, formattingHintsAlignment = sniffAlignment input
, formattingHintsRenderSettings = sniffRenderSettings input
}

extractFieldOrderHint :: String -> [String]
extractFieldOrderHint = map fst . catMaybes . map splitField . lines

sniffAlignment :: String -> Maybe Alignment
sniffAlignment input = case nub . catMaybes . map indentation . catMaybes . map splitField $ lines input of
  [n] -> Just (Alignment n)
  _ -> Nothing
  where

    indentation :: (String, String) -> Maybe Int
    indentation (name, value) = case span isSpace value of
      (_, "") -> Nothing
      (xs, _) -> (Just . succ . length $ name ++ xs)

splitField :: String -> Maybe (String, String)
splitField field = case span isNameChar field of
  (xs, ':':ys) -> Just (xs, ys)
  _ -> Nothing
  where
    isNameChar = (`elem` nameChars)
    nameChars = ['a'..'z'] ++ ['A'..'Z'] ++ "-"

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
sniffRenderSettings input = RenderSettings indentation fieldAlignment commaStyle
  where
    indentation = fromMaybe (renderSettingsIndentation defaultRenderSettings) (sniffIndentation input)
    fieldAlignment = renderSettingsFieldAlignment defaultRenderSettings
    commaStyle = fromMaybe (renderSettingsCommaStyle defaultRenderSettings) (sniffCommaStyle input)
