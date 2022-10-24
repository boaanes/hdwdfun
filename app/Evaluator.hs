module Evaluator where

import           MethodParser

{-
eval :: Expr -> Int
eval (Add x y) = eval x + eval y
eval (Sub x y) = eval x - eval y
eval (Mul x y) = eval x * eval y
eval (Div x y) = eval x `div` eval y
eval (Var v)   = case snd v of
  Just x  -> x
  Nothing -> error "Variable not bound"
eval (Lit x)   = x
-}
