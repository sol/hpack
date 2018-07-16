{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}
module Hpack.Render.Hints (
  FormattingHints (..)
, sniffFormattingHints
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

import           Data.Char
import           Data.Maybe
import           Data.List
import           Control.Applicative

import           Hpack.Render.Dsl

data FormattingHints = FormattingHints {
  formattingHintsFieldOrder :: [String]
, formattingHintsSectionsFieldOrder :: [(String, [String])]
, formattingHintsAlignment :: Maybe Alignment
, formattingHintsRenderSettings :: RenderSettings
} deriving (Eq, Show)

sniffFormattingHints :: [String] -> FormattingHints
sniffFormattingHints (sanitize -> input) = FormattingHints {
  formattingHintsFieldOrder = extractFieldOrder input
, formattingHintsSectionsFieldOrder = extractSectionsFieldOrder input
, formattingHintsAlignment = sniffAlignment input
, formattingHintsRenderSettings = sniffRenderSettings input
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

sniffAlignment :: [String] -> Maybe Alignment
sniffAlignment input = case nub . catMaybes . map indentation . catMaybes . map splitField $ input of
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

sniffRenderSettings :: [String] -> RenderSettings
sniffRenderSettings input = RenderSettings indentation fieldAlignment commaStyle
  where
    indentation = fromMaybe (renderSettingsIndentation defaultRenderSettings) (sniffIndentation input)
    fieldAlignment = renderSettingsFieldAlignment defaultRenderSettings
    commaStyle = fromMaybe (renderSettingsCommaStyle defaultRenderSettings) (sniffCommaStyle input)
