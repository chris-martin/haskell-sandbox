module Main (main) where

import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

import           Data.Monoid              ((<>))

main :: IO ()
main = return ()

data Tup a b = Tup a b

instance Functor (Tup a) where
  fmap f (Tup a b) = Tup a (f b)

instance Monoid a => Applicative (Tup a) where
  (Tup a1 f) <*> (Tup a2 b) = Tup (a1 <> a2) (f b)
  pure b = Tup mempty b

instance Foldable (Tup a) where
  foldr f z (Tup _ b) = f b z
  foldMap f (Tup _ b) = f b

instance Traversable (Tup x) where
  traverse f (Tup x a) = Tup x <$> f a
