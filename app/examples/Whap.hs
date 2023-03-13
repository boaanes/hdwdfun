module Whap
    ( constraintA
    , constraintB
    , m1
    , m2
    , m3
    , m4
    , m5
    , stayArea
    , stayHeight
    , stayPerimeter
    , stayWidth
    ) where
import           Algebra.Graph.AdjacencyMap
import           HotDrink

m1 :: Method
m1 = stars [(VertexMet "m1", [VertexVar "area"]), (VertexVar "width", [VertexMet "m1"]), (VertexVar "height", [VertexMet "m1"])]

m2 :: Method
m2 = stars [(VertexMet "m2", [VertexVar "perimeter"]), (VertexVar "width", [VertexMet "m2"]), (VertexVar "height", [VertexMet "m2"])]

m3 :: Method
m3 = stars [(VertexMet "m3", [VertexVar "height", VertexVar "width"]), (VertexVar "area", [VertexMet "m3"])]

m4 :: Method
m4 = stars [(VertexMet "m4", [VertexVar "height"]), (VertexVar "perimeter", [VertexMet "m4"]), (VertexVar "width", [VertexMet "m4"])]

m5 :: Method
m5 = stars [(VertexMet "m5", [VertexVar "width"]), (VertexVar "perimeter", [VertexMet "m5"]), (VertexVar "height", [VertexMet "m5"])]

constraintA :: Constraint
constraintA = Constraint [m1, m3]

constraintB :: Constraint
constraintB = Constraint [m2, m4, m5]

mArea :: Method
mArea = stars [(VertexMet "mArea", [VertexVar "area"])]

mPerimeter :: Method
mPerimeter = stars [(VertexMet "mPerimeter", [VertexVar "perimeter"])]

mWidth :: Method
mWidth = stars [(VertexMet "mWidth", [VertexVar "width"])]

mHeight :: Method
mHeight = stars [(VertexMet "mHeight", [VertexVar "height"])]

stayArea :: Constraint
stayArea = Constraint [mArea]

stayPerimeter :: Constraint
stayPerimeter = Constraint [mPerimeter]

stayWidth :: Constraint
stayWidth = Constraint [mWidth]

stayHeight :: Constraint
stayHeight = Constraint [mHeight]

