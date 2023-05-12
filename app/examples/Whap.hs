module Whap
    ( constraintA
    , constraintB
    , m1
    , m1Graph
    , m2
    , m2Graph
    , m3
    , m3Graph
    , m4
    , m4Graph
    , m5
    , m5Graph
    , mArea
    , mHeight
    , mPerimeter
    , mWidth
    , stayArea
    , stayHeight
    , stayPerimeter
    , stayWidth
    , vArea
    , vHeight
    , vPerimeter
    , vWidth
    ) where
import           Algebra.Graph.AdjacencyMap
import           HotDrink
import           MethodParser


m1 :: Method
m1 = ("m1", [("area", BinOp "*" (Var "width") (Var "height"))])

m2 :: Method
m2 = ("m2", [("height", UnOp "sqrt" (Var "area")), ("width", UnOp "sqrt" (Var "area"))])

m3 :: Method
m3 = ("m3", [("perimeter", BinOp "*" (Lit (DoubleVal 2)) (BinOp "+" (Var "width") (Var "height")))])

m4 :: Method
m4 = ("m4", [("height", BinOp "-" (Var "width") (BinOp "/" (Var "perimeter") (Lit (DoubleVal 2))))])

m5 :: Method
m5 = ("m5", [("width", BinOp "-" (Var "height") (BinOp "/" (Var "perimeter") (Lit (DoubleVal 2))))])

vArea :: String
vArea = "area"

vPerimeter :: String
vPerimeter = "perimeter"

vWidth :: String
vWidth = "width"

vHeight :: String
vHeight = "height"

m1Graph :: MethodGraph
m1Graph = stars [(VertexMet m1, [VertexVar vArea]), (VertexVar vWidth, [VertexMet m1]), (VertexVar vHeight, [VertexMet m1])]

m2Graph :: MethodGraph
m2Graph = stars [(VertexMet m2, [VertexVar vHeight, VertexVar vWidth]), (VertexVar vArea, [VertexMet m2])]

m3Graph :: MethodGraph
m3Graph = stars [(VertexMet m3, [VertexVar vPerimeter]), (VertexVar vWidth, [VertexMet m3]), (VertexVar vHeight, [VertexMet m3])]

m4Graph :: MethodGraph
m4Graph = stars [(VertexMet m4, [VertexVar vHeight]), (VertexVar vPerimeter, [VertexMet m4]), (VertexVar vWidth, [VertexMet m4])]

m5Graph :: MethodGraph
m5Graph = stars [(VertexMet m5, [VertexVar vWidth]), (VertexVar vPerimeter, [VertexMet m5]), (VertexVar vHeight, [VertexMet m5])]

constraintA :: Constraint
constraintA = Constraint [m1Graph, m3Graph]

constraintB :: Constraint
constraintB = Constraint [m2Graph, m4Graph, m5Graph]

mArea :: MethodGraph
mArea = stars [(VertexMet ("mArea", [("area", Var "area")]), [VertexVar vArea])]

mPerimeter :: MethodGraph
mPerimeter = stars [(VertexMet ("mPerimeter", [("perimeter", Var "perimeter")]), [VertexVar vPerimeter])]

mWidth :: MethodGraph
mWidth = stars [(VertexMet ("mWidth", [("width", Var "width")]), [VertexVar vWidth])]

mHeight :: MethodGraph
mHeight = stars [(VertexMet ("mHeight", [("height", Var "height")]), [VertexVar vHeight])]

stayArea :: Constraint
stayArea = Constraint [mArea]

stayPerimeter :: Constraint
stayPerimeter = Constraint [mPerimeter]

stayWidth :: Constraint
stayWidth = Constraint [mWidth]

stayHeight :: Constraint
stayHeight = Constraint [mHeight]
