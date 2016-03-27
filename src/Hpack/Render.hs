{-# LANGUAGE RecordWildCards #-}
module Hpack.Render where

import           Prelude ()
import           Prelude.Compat

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
, renderSettingsFieldAlignment :: Int
, renderSettingsCommaStyle :: CommaStyle
} deriving (Eq, Show)

defaultRenderSettings :: RenderSettings
defaultRenderSettings = RenderSettings 2 0 LeadingCommas

render :: RenderSettings -> Int -> Element -> [String]
render settings nesting (Stanza name elements) = indent settings nesting name : renderElements elements
  where
    renderElements :: [Element] -> [String]
    renderElements = concatMap (render settings $ succ nesting)
render settings nesting (Field name value) = renderField settings nesting name value

renderField :: RenderSettings -> Int -> String -> Value -> [String]
renderField settings@RenderSettings{..} nesting name value = case renderValue settings value of
  SingleLine "" -> []
  SingleLine x -> [indent settings nesting (name ++ ": " ++ padding ++ x)]
  MultipleLines [] -> []
  MultipleLines xs -> (indent settings nesting name ++ ":") : map (indent settings $ succ nesting) xs
  where
    padding = replicate (renderSettingsFieldAlignment - length name - 2) ' '

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
