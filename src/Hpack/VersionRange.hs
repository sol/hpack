-- | Definitions and parsing of version numbers and version ranges.
module Hpack.VersionRange (Version, VersionRange,
                           parseVersion, parseVersionRange,
                           -- * Internals
                           lexVersionRange, parseVersionCmp,
                           VersionLexeme(..), VersionCmp(..)) where
import Prelude ()
import Prelude.Compat
import Control.Monad ((>=>))
import Data.Char (isDigit)
import Data.Version (Version)
import qualified Data.Version as V
import Text.ParserCombinators.ReadP (readP_to_S)

uncons :: [a] -> Maybe (a, [a])
uncons [] = Nothing
uncons (x:xs) = Just (x,xs)

-- | Read a 'String' of the form @x.y.z@ where @x@, @y@, and @z@ are
-- integers. There must be at least one integer component, but there
-- is no limit as to how many there can be.
parseVersion :: String -> Maybe Version
parseVersion = aux . map fst . filter (null . snd) . readP_to_S V.parseVersion
  where aux [x] = Just x
        aux _ = Nothing

-- | A version range is a predicate on 'Version's
type VersionRange = Version -> Bool

-- | The AST of a version comparison expression
data VersionCmp = VEq Version
                | VLt Version
                | VGt Version
                | VAnd VersionCmp VersionCmp
                | VOr VersionCmp VersionCmp
                | VAny
  deriving (Eq, Show)

-- | The lexemes of a version constraint expression
data VersionLexeme = LexVersion Version
                   | LexEq
                   | LexGt
                   | LexGte
                   | LexLt
                   | LexLte
                   | LexAnd
                   | LexOr
                   | LexLParen
                   | LexRParen
  deriving (Eq, Show)

-- | Attempt to parse a 'VersionRange' predicate
parseVersionRange :: String -> Maybe VersionRange
parseVersionRange =
  lexVersionRange >=> fmap interpretVersionCmp . parseVersionCmp

-- | Interpret a version comparison syntax tree
interpretVersionCmp :: VersionCmp -> VersionRange
interpretVersionCmp VAny _ = True
interpretVersionCmp (VEq v) v' = v == v'
interpretVersionCmp (VLt v) v' = v' < v
interpretVersionCmp (VGt v) v' = v' > v
interpretVersionCmp (VAnd c c') v =
  interpretVersionCmp c v && interpretVersionCmp c' v
interpretVersionCmp (VOr c c') v =
  interpretVersionCmp c v || interpretVersionCmp c' v

-- | Break an input 'String' into a sequence of lexemes.
lexVersionRange :: String -> Maybe [VersionLexeme]
lexVersionRange = (\ws -> if null ws then Just [] else foldMap aux ws) . words
  where aux tok =
          case uncons tok of
            Nothing -> Just []
            Just (h,t)
              | isDigit h ->
                case break (\c -> not (isDigit c || c == '.')) t of
                  (v, rest) -> parseVersion (h:v) >>=
                               flip fmap (aux rest) . (:) . LexVersion
              | h == '>' -> case uncons t of
                              Just ('=', t') -> (LexGte:) <$> aux t'
                              _ -> (LexGt :) <$> aux t
              | h == '<' -> case uncons t of
                              Just ('=', t') -> (LexLte:) <$> aux t'
                              _ -> (LexLt :) <$> aux t
              | h == '=' -> case uncons t of
                              Just ('=', t') -> (LexEq:) <$> aux t'
                              _ -> Nothing
              | h == '&' -> case uncons t of
                              Just ('&', t') -> (LexAnd:) <$> aux t'
                              _ -> Nothing
              | h == '|' -> case uncons t of
                              Just ('|', t') -> (LexOr:) <$> aux t'
                              _ -> Nothing
              | h == '(' -> (LexLParen:) <$> aux t
              | h == ')' -> (LexRParen:) <$> aux t
              | otherwise -> Nothing

-- | Break an input stream on the first unbalanced closing
-- parenthesis. One typically applies this function to an input stream
-- after encountering an opening parenthesis; the returned tuple is
-- the parenthetical and the remaining input.
breakParenthetical :: [VersionLexeme]
                   -> Maybe ([VersionLexeme], [VersionLexeme])
breakParenthetical = go id (0::Int)
  where go acc 0 (LexRParen:ls) = Just (acc [], ls)
        go acc n (l@LexRParen:ls) = go (acc . (l:)) (n-1) ls
        go acc n (l@LexLParen:ls) = go (acc . (l:)) (n+1) ls
        go acc n (l:ls) = go (acc . (l:)) n ls
        go _ _ [] = Nothing

parseVersionCmp :: [VersionLexeme] -> Maybe VersionCmp
parseVersionCmp = go []
  where go [] [] = Just VAny
        go [s] [] = Just s
        go stack lexs =
          case lexs of
            (LexLParen : ls) -> do (v,ls') <- breakParenthetical ls
                                   parseVersionCmp v >>= flip go ls' . (:stack)
            (LexEq : LexVersion v : ls) -> go (VEq v:stack) ls
            (LexEq : _) -> Nothing
            (LexLte : LexVersion v : ls) -> go (VOr (VEq v) (VLt v):stack) ls
            (LexLte : _ ) -> Nothing
            (LexLt : LexVersion v : ls) -> go (VLt v:stack) ls
            (LexLt : _) -> Nothing
            (LexGte : LexVersion v : ls) -> go (VOr (VEq v) (VGt v):stack) ls
            (LexGte : _) -> Nothing
            (LexGt : LexVersion v : ls) -> go (VGt v:stack) ls
            (LexGt : _) -> Nothing
            (LexAnd : ls) -> case stack of
                               [l] -> VAnd l <$> parseVersionCmp ls
                               _ -> Nothing
            (LexOr : ls) -> case stack of
                              [l]-> VOr l <$> parseVersionCmp ls
                              _ -> Nothing
            (LexVersion _ : _) -> Nothing
            (LexRParen : _) -> Nothing
            [] -> Nothing
