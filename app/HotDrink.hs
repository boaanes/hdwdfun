module HotDrink where

import           MethodParser

type Identifier = String
type Priority = Int
type Variable = (Identifier, Maybe Double)
type Method = (Identifier, [(Identifier, Expr)])

data VertexType
  = VertexVar Variable
  | VertexMet Method
  deriving (Eq, Ord, Show)

type Constraint = (([Variable], [Method]), Priority, Bool)
