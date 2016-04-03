module Main (main) where

import           Prelude                  hiding (Left, Right)

import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

main :: IO ()
main = do
  let t = undefined :: PhhhbbtttEither String (Int, String, Char)
  mapM_ quickBatch [functor t, applicative t, monad t]

data PhhhbbtttEither b a = Left a | Right b deriving (Eq, Show)

instance (Eq a, Show a, Eq b, Show b) => EqProp (PhhhbbtttEither b a) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (PhhhbbtttEither b a) where
  arbitrary = oneof [Left <$> arbitrary, Right <$> arbitrary]

instance Functor (PhhhbbtttEither b) where
  fmap f (Left b) = Left (f "x")
  fmap _ (Right a) = Right a

instance Applicative (PhhhbbtttEither a) where
  pure = Left
  Right x <*> _ = Right x
  Left  f <*> x = fmap f x

instance Monad (PhhhbbtttEither a) where
  (Left x)  >>= f = f x
  (Right x) >>= _ = Right x
