module Main (main, l1, l2, a, meh, flipType) where

import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

main :: IO ()
main = return ()

l1 :: Monad m => (a -> b) -> m a -> m b
l1 = (<$>)

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f x y = f <$> x <*> y

a :: Monad m => m a -> m (a -> b) -> m b
a = flip (<*>)

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh xs f = flipType (fmap f xs)

flipType :: (Monad m) => [m a] -> m [a]
flipType = foldr (\x acc -> (:) <$> x <*> acc) (pure [])
