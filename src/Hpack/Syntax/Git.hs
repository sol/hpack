module Hpack.Syntax.Git (
  isValidRef
) where

import           Data.Char (chr)
import           Data.List
import           System.FilePath.Posix

-- https://git-scm.com/docs/git-check-ref-format
isValidRef :: String -> Bool
isValidRef ref =
     not (null ref)
  && not (any (isSuffixOf ".lock") components)
  && not (any (isPrefixOf ".") components)
  && not (".." `isInfixOf` ref)
  && not (any isControl ref)
  && all (`notElem` " ~^:?*[\\") ref
  && not ("//" `isInfixOf` ref)
  && not ("/" `isPrefixOf` ref)
  && not ("/" `isSuffixOf` ref)
  && not ("." `isSuffixOf` ref)
  && not ("@{" `isInfixOf` ref)
  && not (ref == "@")
  where
    components = splitDirectories ref

isControl :: Char -> Bool
isControl c = c < chr 0o040 || c == chr 0o177
