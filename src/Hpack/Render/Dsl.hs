{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Hpack.Render.Dsl (
-- * AST
  Element (..)
, Value (..)

-- * Render
, RenderSettings (..)
, CommaStyle (..)
, defaultRenderSettings
, Alignment (..)
, Nesting
, render

-- * Utils
, sortFieldsBy

#ifdef TEST
, Lines (..)
, IndentOrAlign (..)
, renderValue
, addSortKey
#endif
) where

import           Imports
import           Data.Char (isSpace)

data Element = Stanza String [Element] | Group Element Element | Field String Value | Verbatim String
  deriving (Eq, Show)

data Value =
    Literal String
  | CommaSeparatedList [String]
  | LineSeparatedList [String]
  | WordList [String]
  deriving (Eq, Show)

data Lines = SingleLine String | MultipleLines IndentOrAlign [String]
  deriving (Eq, Show)

data IndentOrAlign =
  Indent
  -- ^
  -- Indent lines, e.g.
  --
  -- description:
  --   some
  --   multiline
  --   description
  |
  Align
  -- ^
  -- Align lines with field labels, e.g.
  --
  -- description: some
  --              multiline
  --              description
  deriving (Eq, Show)

data CommaStyle = LeadingCommas | TrailingCommas
  deriving (Eq, Show)

newtype Nesting = Nesting Int
  deriving (Eq, Show, Num, Enum)

newtype Alignment = Alignment Int
  deriving (Eq, Ord, Show, Num)

data RenderSettings = RenderSettings {
  renderSettingsIndentation :: Int
, renderSettingsFieldAlignment :: Alignment
, renderSettingsCommaStyle :: CommaStyle
, renderSettingsEmptyLinesAsDot :: Bool
} deriving (Eq, Show)

defaultRenderSettings :: RenderSettings
defaultRenderSettings = RenderSettings 2 0 LeadingCommas True

render :: RenderSettings -> Nesting -> Element -> [String]
render settings nesting = \ case
  Stanza name elements -> indent settings nesting name : renderElements settings (succ nesting) elements
  Group a b -> render settings nesting a ++ render settings nesting b
  Field name value -> map (indent settings nesting) $ renderField settings name value
  Verbatim str -> map (indent settings nesting) (lines str)

renderElements :: RenderSettings -> Nesting -> [Element] -> [String]
renderElements settings nesting = concatMap (render settings nesting)

renderField :: RenderSettings -> String -> Value -> [String]
renderField settings@RenderSettings{..} name = renderValue settings >>> \ case
  SingleLine "" -> []
  SingleLine value -> [fieldName ++ value]
  MultipleLines _ [] -> []
  MultipleLines Indent values -> (name ++ ":") : map (indent settings 1) values
  MultipleLines Align (value : values) -> (fieldName ++ value) : map align values
  where
    Alignment fieldAlignment = renderSettingsFieldAlignment

    fieldName :: String
    fieldName = name ++ ": " ++ fieldNamePadding

    fieldNamePadding :: String
    fieldNamePadding = replicate (fieldAlignment - length name - 2) ' '

    align :: String -> String
    align = \ case
      "" -> ""
      value -> padding ++ value

    padding :: String
    padding = replicate (length fieldName) ' '

renderValue :: RenderSettings -> Value -> Lines
renderValue RenderSettings{..} = \ case
  Literal string -> case lines string of
    [value] -> SingleLine value
    values -> MultipleLines Align $ map emptyLineToDot values
  WordList ws -> SingleLine $ unwords ws
  LineSeparatedList xs -> renderLineSeparatedList renderSettingsCommaStyle xs
  CommaSeparatedList xs -> renderCommaSeparatedList renderSettingsCommaStyle xs
  where
    emptyLineToDot :: String -> String
    emptyLineToDot xs
      | isEmptyLine xs && renderSettingsEmptyLinesAsDot = "."
      | otherwise = xs

    isEmptyLine :: String -> Bool
    isEmptyLine = all isSpace

renderLineSeparatedList :: CommaStyle -> [String] -> Lines
renderLineSeparatedList style = MultipleLines Indent . map (padding ++)
  where
    padding = case style of
      LeadingCommas -> "  "
      TrailingCommas -> ""

renderCommaSeparatedList :: CommaStyle -> [String] -> Lines
renderCommaSeparatedList style = MultipleLines Indent . case style of
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

indent :: RenderSettings -> Nesting -> String -> String
indent RenderSettings{..} (Nesting nesting) = \ case
  "" -> ""
  s -> replicate (nesting * renderSettingsIndentation) ' ' ++ s

sortFieldsBy :: [String] -> [Element] -> [Element]
sortFieldsBy existingFieldOrder =
    map snd
  . sortOn fst
  . addSortKey
  . map (\a -> (existingIndex a, a))
  where
    existingIndex :: Element -> Maybe Int
    existingIndex (Field name _) = name `elemIndex` existingFieldOrder
    existingIndex _ = Nothing

addSortKey :: [(Maybe Int, a)] -> [((Int, Int), a)]
addSortKey = go (-1) . zip [0..]
  where
    go :: Int -> [(Int, (Maybe Int, a))] -> [((Int, Int), a)]
    go n xs = case xs of
      [] -> []
      (x, (Just y, a)) : ys -> ((y, x), a) : go y ys
      (x, (Nothing, a)) : ys -> ((n, x), a) : go n ys
