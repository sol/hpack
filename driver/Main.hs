module Main (main) where

import           System.Environment

import qualified Hpack
import qualified Hpack.Config as Hpack

main :: IO ()
main = getArgs >>= Hpack.getOptions Hpack.packageConfig >>= mapM_ (uncurry Hpack.hpack)
