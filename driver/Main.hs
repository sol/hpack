module Main (main) where

import           Run

main :: IO ()
main = run >>= uncurry writeFile
