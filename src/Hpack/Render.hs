module Hpack.Render where

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

class Render a where
  render :: Int -> a -> [String]

instance Render Stanza where
  render nesting (Stanza name fields) = name : renderFields fields
    where
      renderFields :: [Field] -> [String]
      renderFields = concatMap (render $ succ nesting)

instance Render Field where
  render nesting (Field name v) = case renderValue v of
    SingleLine "" -> []
    SingleLine x -> [indent nesting (name ++ ": " ++ x)]
    MultipleLines [] -> []
    MultipleLines xs -> (indent nesting name ++ ":") : map (indent $ succ nesting) xs

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

indent :: Int -> String -> String
indent nesting s = replicate (nesting * 2) ' ' ++ s
