module Evaluator where

import           HotDrink
import           MethodParser

eval :: [Variable] -> Expr -> Double
eval vs (BinOp "+" a b) = eval vs a + eval vs b
eval vs (BinOp "-" a b) = eval vs a - eval vs b
eval vs (BinOp "*" a b) = eval vs a * eval vs b
eval vs (BinOp "/" a b) = eval vs a / eval vs b
eval _ (BinOp {})      = error "Operator not supported"
eval vs (Sqrt e)      = sqrt $ eval vs e
eval vs (Var x)       =
    case lookup x vs of
        Just (Just v) -> v
        _             -> error $ "Variable " <> x <> " not found"
eval _ (Lit x)       = x
