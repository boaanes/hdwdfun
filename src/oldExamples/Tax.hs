module Tax
    ( constraint1
    , constraint2
    , constraint3
    , constraint4
    , constraint5
    , constraint6
    , constraint7
    , stayX1
    , stayX10
    , stayX11
    , stayX12
    , stayX13
    , stayX14
    , stayX15
    , stayX2
    , stayX3
    , stayX4
    , stayX5
    , stayX6
    , stayX7
    , stayX8
    , stayX9
    ) where

import           Algebra.Graph.AdjacencyMap
import           HotDrink

m1 :: Method
m1 = stars [(VertexMet "m1", [VertexVar "x15"]), (VertexVar "x11", [VertexMet "m1"]), (VertexVar "x14", [VertexMet "m1"])]

m2 :: Method
m2 = stars [(VertexMet "m2", [VertexVar "x11"]), (VertexVar "x14", [VertexMet "m2"]), (VertexVar "x15", [VertexMet "m2"])]

m3 :: Method
m3 = stars [(VertexMet "m3", [VertexVar "x14"]), (VertexVar "x11", [VertexMet "m3"]), (VertexVar "x15", [VertexMet "m3"])]

constraint1 :: Constraint
constraint1 = Constraint [m1, m2, m3]

m4 :: Method
m4 = stars [(VertexMet "m4", [VertexVar "x11"]), (VertexVar "x9", [VertexMet "m4"]), (VertexVar "x12", [VertexMet "m4"]), (VertexVar "x10", [VertexMet "m4"]), (VertexVar "x4", [VertexMet "m4"]), (VertexVar "x2", [VertexMet "m4"])]

m5 :: Method
m5 = stars [(VertexMet "m5", [VertexVar "x9"]), (VertexVar "x11", [VertexMet "m5"]), (VertexVar "x12", [VertexMet "m5"]), (VertexVar "x10", [VertexMet "m5"]), (VertexVar "x4", [VertexMet "m5"]), (VertexVar "x12", [VertexMet "m5"])]

m6 :: Method
m6 = stars [(VertexMet "m6", [VertexVar "x4"]), (VertexVar "x11", [VertexMet "m6"]), (VertexVar "x2", [VertexMet "m6"]), (VertexVar "x10", [VertexMet "m6"]), (VertexVar "x9", [VertexMet "m6"]), (VertexVar "x12", [VertexMet "m6"])]

m7 :: Method
m7 = stars [(VertexMet "m7", [VertexVar "x12"]), (VertexVar "x11", [VertexMet "m7"]), (VertexVar "x10", [VertexMet "m7"]), (VertexVar "x4", [VertexMet "m7"]), (VertexVar "x2", [VertexMet "m7"]), (VertexVar "x9", [VertexMet "m7"])]

m8 :: Method
m8 = stars [(VertexMet "m8", [VertexVar "x10"]), (VertexVar "x11", [VertexMet "m8"]), (VertexVar "x12", [VertexMet "m8"]), (VertexVar "x4", [VertexMet "m8"]), (VertexVar "x2", [VertexMet "m8"]), (VertexVar "x9", [VertexMet "m8"])]

m9 :: Method
m9 = stars [(VertexMet "m9", [VertexVar "x2"]), (VertexVar "x11", [VertexMet "m9"]), (VertexVar "x4", [VertexMet "m9"]), (VertexVar "x10", [VertexMet "m9"]), (VertexVar "x9", [VertexMet "m9"]), (VertexVar "x12", [VertexMet "m9"])]

constraint2 :: Constraint
constraint2 = Constraint [m4, m5, m6, m7, m8, m9]

m10 :: Method
m10 = stars [(VertexMet "m10", [VertexVar "x4"])]

m11 :: Method
m11 = stars [(VertexMet "m11", [VertexVar "x5"]), (VertexVar "x4", [VertexMet "m11"])]

constraint3 :: Constraint
constraint3 = Constraint [m10, m11]

m12 :: Method
m12 = stars [(VertexMet "m12", [VertexVar "x2"]), (VertexVar "x3", [VertexMet "m12"])]

m13 :: Method
m13 = stars [(VertexMet "m13", [VertexVar "x3"]), (VertexVar "x2", [VertexMet "m13"])]

constraint4 :: Constraint
constraint4 = Constraint [m12, m13]

m14 :: Method
m14 = stars [(VertexMet "m14", [VertexVar "x8"]), (VertexVar "x11", [VertexMet "m14"]), (VertexVar "x6", [VertexMet "m14"])]

m15 :: Method
m15 = stars [(VertexMet "m15", [VertexVar "x11"]), (VertexVar "x8", [VertexMet "m15"]), (VertexVar "x6", [VertexMet "m15"])]

m16 :: Method
m16 = stars [(VertexMet "m16", [VertexVar "x6"]), (VertexVar "x11", [VertexMet "m16"]), (VertexVar "x8", [VertexMet "m16"])]

constraint5 :: Constraint
constraint5 = Constraint [m14, m15, m16]

m17 :: Method
m17 = stars [(VertexMet "m17", [VertexVar "x7"]), (VertexVar "x6", [VertexMet "m17"])]

m18 :: Method
m18 = stars [(VertexMet "m18", [VertexVar "x6"]), (VertexVar "x7", [VertexMet "m18"])]

constraint6 :: Constraint
constraint6 = Constraint [m17, m18]

m19 :: Method
m19 = stars [(VertexMet "m19", [VertexVar "x12"]), (VertexVar "x1", [VertexMet "m19"]), (VertexVar "x13", [VertexMet "m19"])]

m20 :: Method
m20 = stars [(VertexMet "m20", [VertexVar "x13"]), (VertexVar "x12", [VertexMet "m20"]), (VertexVar "x1", [VertexMet "m20"])]

constraint7 :: Constraint
constraint7 = Constraint [m19, m20]

-- stay constraints

mx1 :: Method
mx1 = stars [(VertexMet "mx1", [VertexVar "x1"])]

mx2 :: Method
mx2 = stars [(VertexMet "mx2", [VertexVar "x2"])]

mx3 :: Method
mx3 = stars [(VertexMet "mx3", [VertexVar "x3"])]

mx4 :: Method
mx4 = stars [(VertexMet "mx4", [VertexVar "x4"])]

mx5 :: Method
mx5 = stars [(VertexMet "mx5", [VertexVar "x5"])]

mx6 :: Method
mx6 = stars [(VertexMet "mx6", [VertexVar "x6"])]

mx7 :: Method
mx7 = stars [(VertexMet "mx7", [VertexVar "x7"])]

mx8 :: Method
mx8 = stars [(VertexMet "mx8", [VertexVar "x8"])]

mx9 :: Method
mx9 = stars [(VertexMet "mx9", [VertexVar "x9"])]

mx10 :: Method
mx10 = stars [(VertexMet "mx10", [VertexVar "x10"])]

mx11 :: Method
mx11 = stars [(VertexMet "mx11", [VertexVar "x11"])]

mx12 :: Method
mx12 = stars [(VertexMet "mx12", [VertexVar "x12"])]

mx13 :: Method
mx13 = stars [(VertexMet "mx13", [VertexVar "x13"])]

mx14 :: Method
mx14 = stars [(VertexMet "mx14", [VertexVar "x14"])]

mx15 :: Method
mx15 = stars [(VertexMet "mx15", [VertexVar "x15"])]

stayX1 :: Constraint
stayX1 = Constraint [mx1]

stayX2 :: Constraint
stayX2 = Constraint [mx2]

stayX3 :: Constraint
stayX3 = Constraint [mx3]

stayX4 :: Constraint
stayX4 = Constraint [mx4]

stayX5 :: Constraint
stayX5 = Constraint [mx5]

stayX6 :: Constraint
stayX6 = Constraint [mx6]

stayX7 :: Constraint
stayX7 = Constraint [mx7]

stayX8 :: Constraint
stayX8 = Constraint [mx8]

stayX9 :: Constraint
stayX9 = Constraint [mx9]

stayX10 :: Constraint
stayX10 = Constraint [mx10]

stayX11 :: Constraint
stayX11 = Constraint [mx11]

stayX12 :: Constraint
stayX12 = Constraint [mx12]

stayX13 :: Constraint
stayX13 = Constraint [mx13]

stayX14 :: Constraint
stayX14 = Constraint [mx14]

stayX15 :: Constraint
stayX15 = Constraint [mx15]

