-- | https://stackoverflow.com/questions/36757667/how-do-i-use-ord-and-chr-with-only-a-to-z-and-0-to-9
module Main (main) where

import           Control.Monad (guard)
import           Data.Maybe    (fromJust)
import           Data.Word     (Word8)

{-
  Define a type CaesarChar that corresponds to a valid character
  and has Ord defined the way you want it.
-}

newtype CaesarChar = CaesarChar { caesarCharIndex :: Word8 }
  deriving (Eq, Ord, Show)

fromChar :: Char -> Maybe CaesarChar
fromChar c | c >= 'A' && c <= 'Z' =
  caesarChar $ fromEnum c - fromEnum 'A'
fromChar c | c >= '0' && c <= '9' =
  caesarChar $ fromEnum c - fromEnum '0' + length ['A'..'Z']

caesarChar :: Integral i => i -> Maybe CaesarChar
caesarChar i = do
  let i' = fromIntegral i
  guard $ i' >= caesarCharIndex minBound
  guard $ i' <= caesarCharIndex maxBound
  return $ CaesarChar i'

instance Enum CaesarChar where
  toEnum = fromJust . caesarChar
  fromEnum = fromIntegral . caesarCharIndex

instance Bounded CaesarChar where
  minBound = CaesarChar 0
  maxBound = CaesarChar $ fromIntegral $ length ['A'..'Z'] + length ['0'..'9']

{-
  Then the cipher implementation is completely generic.
-}

-- | Apply the cipher to a single character
caesar1 :: (Bounded a, Enum a, Integral i) => i -> a -> a
caesar1 i a = toEnum $ (fromEnum a + fromIntegral i) `mod`
  (fromIntegral $ fromEnum (maxBound :: CaesarChar))

-- | Apply the cipher to multiple characters.
caesarN :: (Bounded a, Enum a, Functor f, Integral i) => i -> f a -> f a
caesarN i = fmap $ caesar1 i

-- | Apply the cipher only to some characters.
caesarNMaybe :: (Bounded a, Enum a, Functor f, Integral i) =>
  (b -> Maybe a) -> (a -> b) -> i -> f b -> f a
caesarNMaybe f i bs = undefined <$> bs

main :: IO ()
main = putStrLn $ caesarNMaybe caesarChar 2 "ABYZ 0189"
