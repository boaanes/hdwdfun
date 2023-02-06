module Whap
    ( area
    , constraints
    , exampleGraph
    , exampleGraph2
    , height
    , m1
    , m2
    , m3
    , m4
    , m5
    , perimeter
    , width
    ) where

import           Algebra.Graph
import           HotDrink
import           MethodParser

-- Below is example data for the whap example

width :: Variable
width = ("width", Just 10)

height :: Variable
height = ("height", Just 2)

area :: Variable
area = ("area", Just 100)

perimeter :: Variable
perimeter = ("perimeter", Just 40)

m1 :: Method
m1 = ("m1", [("area", BinOp "*" (Var "width") (Var "height"))])

m2 :: Method
m2 = ("m2", [("perimeter", BinOp "*" (Lit 2) (BinOp "+" (Var "width") (Var "height")))])

m3 :: Method
m3 = ("m3", [("width", Sqrt (Var "area")), ("height", Sqrt (Var "area"))])

m4 :: Method
m4 = ("m4", [("height", BinOp "-" (BinOp "/" (Var "perimeter") (Lit 2)) (Var "width"))])

m5 :: Method
m5 = ("m5", [("width", BinOp "-" (BinOp "/" (Var "perimeter") (Lit 2)) (Var "height"))])

width2 :: Variable
width2 = ("width", Just 20)

height2 :: Variable
height2 = ("height", Just 20)

constraintA :: Constraint
constraintA = (([width, area, height], [m1]), 1, False)

constraintB :: Constraint
constraintB = (([width, height, perimeter], [m2, m4, m5]), 2, False)

constraints :: [Constraint]
constraints = [constraintA, constraintB]

exampleAdjList :: [(VertexType, [VertexType])]
exampleAdjList = [(VertexVar width, [VertexMet m1, VertexMet m4, VertexMet m2]), (VertexVar area, [VertexMet m3]), (VertexVar height, [VertexMet m2, VertexMet m1, VertexMet m5]), (VertexVar perimeter, [VertexMet m5, VertexMet m4]), (VertexMet m5, [VertexVar width]), (VertexMet m1, [VertexVar area]), (VertexMet m2, [VertexVar perimeter]), (VertexMet m3, [VertexVar width, VertexVar height]), (VertexMet m4, [VertexVar height])]

exampleAdjList2 :: [(VertexType, [VertexType])]
exampleAdjList2 = [(VertexVar width2, [VertexMet m1, VertexMet m4, VertexMet m2]), (VertexVar area, [VertexMet m3]), (VertexVar height2, [VertexMet m2, VertexMet m1, VertexMet m5]), (VertexVar perimeter, [VertexMet m5, VertexMet m4]), (VertexMet m5, [VertexVar width2]), (VertexMet m1, [VertexVar area]), (VertexMet m2, [VertexVar perimeter]), (VertexMet m3, [VertexVar width2, VertexVar height2]), (VertexMet m4, [VertexVar height2])]

exampleGraph :: Graph VertexType
exampleGraph = stars exampleAdjList

exampleGraph2 :: Graph VertexType
exampleGraph2 = stars exampleAdjList2
