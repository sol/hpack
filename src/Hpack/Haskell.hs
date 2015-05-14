module Hpack.Haskell (
  isModule
, isQualifiedIdentifier
, isIdentifier
) where

import           Data.Char

isModule :: [String] -> Bool
isModule name = (not . null) name && all isModuleName name

isModuleName :: String -> Bool
isModuleName name = case name of
  x : xs -> isUpper x && all isIdChar xs
  _ -> False

isQualifiedIdentifier :: [String] -> Bool
isQualifiedIdentifier name = case reverse name of
  x : xs  -> isIdentifier x && isModule xs
  _ -> False

isIdentifier :: String -> Bool
isIdentifier name = case name of
  x : xs -> isLower x && all isIdChar xs && name `notElem` reserved
  _ -> False

reserved :: [String]
reserved = [
    "case"
  , "class"
  , "data"
  , "default"
  , "deriving"
  , "do"
  , "else"
  , "foreign"
  , "if"
  , "import"
  , "in"
  , "infix"
  , "infixl"
  , "infixr"
  , "instance"
  , "let"
  , "module"
  , "newtype"
  , "of"
  , "then"
  , "type"
  , "where"
  , "_"
  ]

isIdChar :: Char -> Bool
isIdChar c = isAlphaNum c || c == '_' || c == '\''
