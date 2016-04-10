module Main (main, factorial) where

import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

main :: IO ()
main = return ()

factorial :: Integer -> Integer
factorial n = case compare n 0 of
  LT -> -1
  GT -> n * factorial (n-1)
  EQ -> 1
