module Main (main) where

import           Cabalize

main :: IO ()
main = cabalize >>= uncurry writeFile
