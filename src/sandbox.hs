module Main (main, Expr(..), beta) where

import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

import qualified Data.List
import qualified Data.Maybe

main :: IO ()
main = return ()

data Expr = C Float | Expr :+ Expr | Expr :- Expr | Expr :* Expr | Expr :/ Expr
          | Var String | Let [(String, Expr)] Expr

-- | @beta var value e@ replaces variables named @var@ with the value @value@
-- wherever anywhere that variable occurs in expression @e@.
beta :: String -> Expr -> Expr -> Expr

-- "let x = y in x" = y
beta v1 value (Var v2) | v1 == v2 = value

-- "let x = y in z" = z
beta _ _ e@(Var _) = e

-- Constants are unaffected
beta _ _ c@(C _) = c

-- For operators, apply @beta a b@ recursively to the operands.
beta a b (e1 :+ e2) = (beta a b e1) :+ (beta a b e2)
beta a b (e1 :- e2) = (beta a b e1) :- (beta a b e2)
beta a b (e1 :* e2) = (beta a b e1) :* (beta a b e2)
beta a b (e1 :/ e2) = (beta a b e1) :/ (beta a b e2)

beta a b e@(Let bindings body) =

    -- The variable is shadowed by a let binding, so return
    -- the original expression unmodified.
    if bindingsContains a bindings then e

    -- Apply @beta a b@ recursively to the body of the let expression.
    else Let bindings (beta a b body)

bindingsContains :: String -> [(String, Expr)] -> Bool
bindingsContains x bindings = Data.Maybe.isJust $ Data.List.find ((== x) . fst) bindings
