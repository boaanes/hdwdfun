module Evaluator where

import           Algebra.Graph
import           GraphHelpers
import           HotDrink
import           MethodParser


eval :: Graph VertexType -> Expr -> Int
eval g (Add x y) = eval g x + eval g y
eval g (Sub x y) = eval g x - eval g y
eval g (Mul x y) = eval g x * eval g y
eval g (Div x y) = eval g x `div` eval g y
eval g (Sqrt x)  = floor $ sqrt $ fromIntegral $ eval g x
eval g (Var x)   =
  case lookupLabel x g of
    Just (VertexVar (_, Just v))  -> v
    Just (VertexVar (_, Nothing)) -> error "Variable not set"
    Just (VertexMet _)            -> error "Identifier is a method"
    Nothing                       -> error "Variable not found"
eval _ (Lit x)   = x
