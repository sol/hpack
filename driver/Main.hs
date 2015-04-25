module Main (main) where

import           Prelude ()
import           Prelude.Compat
import           Control.Monad
import           Control.DeepSeq
import           Control.Exception
import           System.IO.Error

import           Run

main :: IO ()
main = do
  (name, new) <- run
  old <- force . either (const Nothing) Just <$> tryJust (guard . isDoesNotExistError) (readFile name)
  unless (old == Just new) (writeFile name new)
