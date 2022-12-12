module WhapStay
    ( area
    , constraintStayArea
    , constraintStayPerimeter
    , constraints
    , exampleGraph
    , exampleGraph2
    , height
    , m1
    , m2
    , m4
    , m5
    , perimeter
    , stayArea
    , stayPerimeter
    , width
    ) where

import           Algebra.Graph
import           HotDrink
import           MethodParser

width :: Variable
width = ("width", Just 10)

height :: Variable
height = ("height", Just 10)

area :: Variable
area = ("area", Just 100)

perimeter :: Variable
perimeter = ("perimeter", Just 40)

m1 :: Method
m1 = ("m1", [("area", Bin "*" (Var "width") (Var "height"))])

m2 :: Method
m2 = ("m2", [("perimeter", Bin "*" (Lit 2) (Bin "+" (Var "width") (Var "height")))])

m3 :: Method
m3 = ("m3", [("width", Sqrt (Var "area")), ("height", Sqrt (Var "area"))])

m4 :: Method
m4 = ("m4", [("height", Bin "-" (Bin "/" (Var "perimeter") (Lit 2)) (Var "width"))])

m5 :: Method
m5 = ("m5", [("width", Bin "-" (Bin "/" (Var "perimeter") (Lit 2)) (Var "height"))])

stayWidth :: Method
stayWidth = ("stayWidth", [("width", Var "width")])

stayHeight :: Method
stayHeight = ("stayHeight", [("height", Var "height")])

stayArea :: Method
stayArea = ("stayArea", [("area", Var "area")])

stayPerimeter :: Method
stayPerimeter = ("stayPerimeter", [("perimeter", Var "perimeter")])

width2 :: Variable
width2 = ("width", Just 20)

height2 :: Variable
height2 = ("height", Just  2)

constraintArea :: Constraint
constraintArea = (([width, area, height], [m1]), 0)

constraintPerimeter :: Constraint
constraintPerimeter = (([width, height, perimeter], [m2, m4, m5]), 1)

constraintStayWidth :: Constraint
constraintStayWidth = (([width], [stayWidth]), 2)

constraintStayHeight :: Constraint
constraintStayHeight = (([height], [stayHeight]), 3)

constraintStayArea :: Constraint
constraintStayArea = (([area], [stayArea]), 4)

constraintStayPerimeter :: Constraint
constraintStayPerimeter = (([perimeter], [stayPerimeter]), 5)

constraints :: [Constraint]
constraints = [constraintArea, constraintPerimeter, constraintStayWidth, constraintStayHeight, constraintStayArea, constraintStayPerimeter]

exampleAdjList :: [(VertexType, [VertexType])]
exampleAdjList = [(VertexVar width, [VertexMet m1, VertexMet m4, VertexMet m2]), (VertexVar area, [VertexMet m3]), (VertexVar height, [VertexMet m2, VertexMet m1, VertexMet m5]), (VertexVar perimeter, [VertexMet m5, VertexMet m4]), (VertexMet m5, [VertexVar width]), (VertexMet m1, [VertexVar area]), (VertexMet m2, [VertexVar perimeter]), (VertexMet m3, [VertexVar width, VertexVar height]), (VertexMet m4, [VertexVar height]), (VertexMet stayWidth, [VertexVar width]), (VertexMet stayHeight, [VertexVar height]), (VertexMet stayArea, [VertexVar area]), (VertexMet stayPerimeter, [VertexVar perimeter])]

exampleAdjList2 :: [(VertexType, [VertexType])]
exampleAdjList2 = [(VertexVar width2, [VertexMet m1, VertexMet m4, VertexMet m2]), (VertexVar area, [VertexMet m3]), (VertexVar height2, [VertexMet m2, VertexMet m1, VertexMet m5]), (VertexVar perimeter, [VertexMet m5, VertexMet m4]), (VertexMet m5, [VertexVar width2]), (VertexMet m1, [VertexVar area]), (VertexMet m2, [VertexVar perimeter]), (VertexMet m3, [VertexVar width2, VertexVar height2]), (VertexMet m4, [VertexVar height2])]

exampleGraph :: Graph VertexType
exampleGraph = stars exampleAdjList

exampleGraph2 :: Graph VertexType
exampleGraph2 = stars exampleAdjList2
