module Main (main) where

import           Prelude

import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

type X = Int

main :: IO ()
main = do
  let t = undefined :: Identity (X, X, X)
  mapM_ quickBatch [functor t, applicative t, monad t]

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance (Eq a, Show a) => EqProp (Identity a) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity x = Identity (f x)

instance Monad Identity where
  Identity x >>= f = f x
