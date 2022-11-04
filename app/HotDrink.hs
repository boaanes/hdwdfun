module HotDrink where

import           MethodParser

type Identifier = String
type Variable = (Identifier, Maybe Int)
type Method = (Identifier, Identifier, Expr)

data VertexType
  = VertexVar Variable
  | VertexMet Method
  deriving (Eq, Ord, Show)

type Constraint = ([Variable], [Method])
