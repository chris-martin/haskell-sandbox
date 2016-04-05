module Main (main, Expr(..), sub) where

import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

import qualified Data.List
import qualified Data.Maybe

main :: IO ()
main = return ()

data Expr = C Float | V String
          | Let [(String, Expr)] Expr
          | Expr :+ Expr | Expr :- Expr
          | Expr :* Expr | Expr :/ Expr
            deriving Show

-- | @sub var value e@ replaces variables named @var@ with the value @value@
-- wherever anywhere that variable occurs in expression @e@.
sub :: String -> Expr -> Expr -> Expr

-- "let x = y in x" = y
sub v1 value (V v2) | v1 == v2 = value

-- "let x = y in z" = z
sub _ _ e@(V _) = e

-- Constants are unaffected
sub _ _ c@(C _) = c

-- For operators, apply @sub a b@ recursively to the operands.
sub a b (e1 :+ e2) = (sub a b e1) :+ (sub a b e2)
sub a b (e1 :- e2) = (sub a b e1) :- (sub a b e2)
sub a b (e1 :* e2) = (sub a b e1) :* (sub a b e2)
sub a b (e1 :/ e2) = (sub a b e1) :/ (sub a b e2)

-- The variable is shadowed by a let binding, so only substitute
-- into the bindings, and leave the body expression unmodified.
sub a b (Let bindings e) | bindingsContains a bindings =
    Let (subIntoBindings a b bindings) e

-- Apply @sub a b@ recursively to the body of the let expression.
sub a b (Let bindings body) =
    Let (subIntoBindings a b bindings) (sub a b body)

bindingsContains :: String -> [(String, Expr)] -> Bool
bindingsContains x bindings =
    Data.Maybe.isJust $ Data.List.find ((== x) . fst) bindings

subIntoBindings :: String -> Expr -> [(a, Expr)] -> [(a, Expr)]
subIntoBindings a b bindings = (fmap . fmap) (sub a b) bindings
