module HotDrink where

import           Algebra.Graph

data Expr
  = Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Sqrt Expr
  | Var Variable
  | Lit Int
  deriving (Show, Eq)

type Identifier = String
type Variable = (Identifier, Maybe Int)
type Method = String

type OtherMethod = (Identifier, [Variable], [Variable], [Variable] -> [Variable])

data VertexType = VertexVar Variable | VertexMet Method deriving (Show, Eq, Ord)

type Constraint = ([Variable], [Method])

eval :: Expr -> Int
eval (Add x y) = eval x + eval y
eval (Sub x y) = eval x - eval y
eval (Mul x y) = eval x * eval y
eval (Div x y) = eval x `div` eval y
eval (Sqrt x)  = floor . sqrt . fromIntegral $ eval x
eval (Var v)   = case snd v of
  Just x  -> x
  Nothing -> error "Variable not bound"
eval (Lit x)   = x

-- Below is example data for the whap example

w :: Variable
w = ("w", Just 10)

h :: Variable
h = ("h", Just 10)

a :: Variable
a = ("A", Just 100)

p :: Variable
p = ("p", Just 40)

m1 :: Method
m1 = "w, h -> a"

m2 :: Method
m2 = "w, h -> p"

m3 :: Method
m3 = "a -> w, h"

m4 :: Method
m4 = "p, w -> h"

m5 :: Method
m5 = "p, h -> w"

constraintA :: Constraint
constraintA = ([w, a, h], [m1, m3])

constraintB :: Constraint
constraintB = ([w, h, p], [m2, m4, m5])

constraints :: [Constraint]
constraints = [constraintA, constraintB]

exampleAdjList :: [(VertexType, [VertexType])]
exampleAdjList = [(VertexVar w, [VertexMet m1, VertexMet m4, VertexMet m2]), (VertexVar a, [VertexMet m3]), (VertexVar h, [VertexMet m2, VertexMet m1, VertexMet m5]), (VertexVar p, [VertexMet m5, VertexMet m4]), (VertexMet m5, [VertexVar w]), (VertexMet m3, [VertexVar w, VertexVar h]), (VertexMet m1, [VertexVar a]), (VertexMet m2, [VertexVar p]), (VertexMet m4, [VertexVar h])]

exampleGraph :: Graph VertexType
exampleGraph = stars exampleAdjList

exampleExpr :: Expr
exampleExpr = Div (Mul (Add (Var w) (Var h)) (Var a)) (Lit 5)
