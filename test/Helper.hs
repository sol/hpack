module Helper (
  module Test.Hspec
, module Test.Mockery.Directory
, module Control.Applicative
, shouldBeOneOf
, shouldReturnOneOf
) where

import           Test.Hspec
import           Test.HUnit
import           Test.Mockery.Directory
import           Control.Applicative
import           Control.Monad (unless)
import           Data.List (intercalate)

expectTrue :: String -> Bool -> IO ()
expectTrue msg b = unless b (Test.HUnit.assertFailure msg)

shouldBeOneOf :: (Eq a, Show a) => a -> [a] -> IO ()
x `shouldBeOneOf` xs =
  expectTrue ("expected: " ++ show x ++ "\n to be one of:\n" ++ intercalate "\n" (map (("  " ++) . show) xs))
             (x `elem` xs)

shouldReturnOneOf :: (Show a, Eq a) => IO a -> [a] -> IO ()
action `shouldReturnOneOf` expected = action >>= (`shouldBeOneOf` expected)