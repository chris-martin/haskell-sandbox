module Main (main, Nullable, isNull) where

import           Data.IORef

x :: IO (IORef (Maybe (IORef (Maybe Bool))))
x = newIORef . Just =<< newIORef Nothing

main :: IO ()
main = undefined

data Nullable a = Null | Val a

isNull :: Nullable a -> Bool
isNull Null = True
isNull _ = False

fold :: b -> (a -> b) -> Nullable a -> b
fold = undefined
