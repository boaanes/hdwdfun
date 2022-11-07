module Evaluator where

import           Algebra.Graph
import           GraphHelpers
import           HotDrink
import           MethodParser


eval :: Graph VertexType -> Expr -> Double
eval g (Bin "+" a b) = eval g a + eval g b
eval g (Bin "-" a b) = eval g a - eval g b
eval g (Bin "*" a b) = eval g a * eval g b
eval g (Bin "/" a b) = eval g a / eval g b
eval _ (Bin {})      = error "Operator not supported"
eval g (Var x)       =
  case lookupLabel x g of
    Just (VertexVar (_, Just v))  -> v
    Just (VertexVar (_, Nothing)) -> error "Variable not set"
    Just (VertexMet _)            -> error "Identifier is a method"
    Nothing                       -> error "Variable not found"
eval _ (Lit x)       = x
