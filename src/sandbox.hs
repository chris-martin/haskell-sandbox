module Main (main) where

main :: IO ()
main = mapM_ (putStrLn . show . isPowOf2) [1..10]

isPowOf2 :: Int -> Maybe Int
isPowOf2 n =
    if (x `mod` 1.0 /= 0.0) then Nothing
    else Just (truncate x)
  where
    x = logBase (fromIntegral 2) (fromIntegral n)
