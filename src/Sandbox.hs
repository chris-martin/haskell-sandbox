module Sandbox where

import qualified Data.Aeson as A
import Data.Semigroup ((<>))
import Data.Sequence (Seq, (|>), (<|))
import qualified Data.Sequence as Seq
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Vector (Vector)

data Atom = Atom'Text Text
          | Atom'Num Scientific
          | Atom'Bool Bool
          | Atom'Null

class Val a where val :: a -> A.Value
instance Val A.Object where val = A.Object
instance Val A.Array where val = A.Array
instance Val Atom where val = \case
  Atom'Text x -> A.String x
  Atom'Num  x -> A.Number x
  Atom'Bool x -> A.Bool   x
  Atom'Null   -> A.Null

data Err = Err'Or (Seq Err)
         | Err'And (Seq Err)
         | Err Text A.Value

err'or :: Err -> Err -> Err
err'or (Err'Or xs) (Err'Or ys) -> Err'Or $ xs <> ys
err'or (Err'Or xs) y           -> Err'Or $ xs |> y
err'or y           (Err'Or xs) -> Err'Or $ y <| xs
err'or x           y           -> Err'Or $ Seq.fromList [x, y]

err'and :: Err -> Err -> Err
err'and (Err'And xs) (Err'And ys) -> Err'And $ xs <> ys
err'and (Err'And xs) y            -> Err'And $ xs |> y
err'and y            (Err'And xs) -> Err'And $ y <| xs
err'and x            y            -> Err'And $ Seq.fromList [x, y]

data Format j a
  { enc :: a -> j
  , dec :: j -> Either Err a
  }

fmt'or :: Format j a -> Format j a -> Format j a
Format enc dec1 `fmt'or` Format _ dec2 = Format{..}
  where
    dec j = case dec1 j of Right a -> Right a
                           Left e1 -> case dec2 j of Right a -> Right a
                                                     Left e2 -> err'or e1 e2
fmt'id :: Format a a
fmt'id = Format id Right

fmt'text :: Format A.Value Text
fmt'text = Format A.String dec
  where
    dec (A.String x) = Right x
    dec x = Left $ Err "Expected text" x
