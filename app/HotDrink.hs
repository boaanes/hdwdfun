module HotDrink where

import           Algebra.Graph
import           MethodParser

type Identifier = String
type Variable = (Identifier, Maybe Int)
type Method = (Identifier, [Identifier], Identifier, Expr)

data VertexType = VertexVar Variable | VertexMet Method deriving (Show, Eq, Ord)

type Constraint = ([Variable], [Method])

-- Below is example data for the whap example

width :: Variable
width = ("width", Just 10)

height :: Variable
height = ("height", Just 10)

area :: Variable
area = ("area", Just 100)

perimeter :: Variable
perimeter = ("perimeter", Just 40)

m1 :: Method
m1 = ("m1", ["width", "height"], "area", Mul (Var "width") (Var "height"))

m2 :: Method
m2 = ("m2", ["width", "height"], "perimeter", Mul (Lit 2) (Add (Var "width") (Var "height")))

m4 :: Method
m4 = ("m4", ["perimeter", "width"], "height", Sub (Div (Var "perimeter") (Lit 2)) (Var "width"))

m5 :: Method
m5 = ("m5", ["perimeter", "height"], "width", Sub (Div (Var "perimeter") (Lit 2)) (Var "height"))

constraintA :: Constraint
constraintA = ([width, area, height], [m1])

constraintB :: Constraint
constraintB = ([width, height, perimeter], [m2, m4, m5])

constraints :: [Constraint]
constraints = [constraintA, constraintB]

exampleAdjList :: [(VertexType, [VertexType])]
exampleAdjList = [(VertexVar width, [VertexMet m1, VertexMet m4, VertexMet m2]), (VertexVar area, []), (VertexVar height, [VertexMet m2, VertexMet m1, VertexMet m5]), (VertexVar perimeter, [VertexMet m5, VertexMet m4]), (VertexMet m5, [VertexVar width]), (VertexMet m1, [VertexVar area]), (VertexMet m2, [VertexVar perimeter]), (VertexMet m4, [VertexVar height])]

width2 :: Variable
width2 = ("width", Just 20)

height2 :: Variable
height2 = ("height", Just 20)

exampleAdjList2 :: [(VertexType, [VertexType])]
exampleAdjList2 = [(VertexVar width2, [VertexMet m1, VertexMet m4, VertexMet m2]), (VertexVar area, []), (VertexVar height2, [VertexMet m2, VertexMet m1, VertexMet m5]), (VertexVar perimeter, [VertexMet m5, VertexMet m4]), (VertexMet m5, [VertexVar width2]), (VertexMet m1, [VertexVar area]), (VertexMet m2, [VertexVar perimeter]), (VertexMet m4, [VertexVar height2])]

exampleGraph :: Graph VertexType
exampleGraph = stars exampleAdjList

exampleGraph2 :: Graph VertexType
exampleGraph2 = stars exampleAdjList2
