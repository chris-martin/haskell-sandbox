module Main (main) where

import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

main :: IO ()
main = do
  let t = undefined :: Nope (Int, String, Char)
  mapM_ quickBatch [functor t, applicative t, monad t]

---------------------------------

data Nope a = NopeDotJpg deriving (Eq, Show)

instance EqProp (Nope a) where
  (=-=) = eq

instance Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

-- | >>> "hi"
-- "hi"
instance Functor Nope where
  fmap  _ _ = NopeDotJpg

instance Applicative Nope where
  pure  _   = NopeDotJpg
  (<*>) _ _ = NopeDotJpg

instance Monad Nope where
  (>>=) _ _ = NopeDotJpg

--------------------------------
