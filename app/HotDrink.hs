module HotDrink where

import           Algebra.Graph

type Identifier = String
type Variable = (Identifier, Maybe Int)
type Method = String

type OtherMethod = (Identifier, [Variable], [Variable], Int -> Int)

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
m1 = "width, height -> area"

m2 :: Method
m2 = "width, height -> perimeter"

m3 :: Method
m3 = "a -> width, height"

m4 :: Method
m4 = "p, width -> height"

m5 :: Method
m5 = "p, height -> width"

constraintA :: Constraint
constraintA = ([width, area, height], [m1, m3])

constraintB :: Constraint
constraintB = ([width, height, perimeter], [m2, m4, m5])

constraints :: [Constraint]
constraints = [constraintA, constraintB]

exampleAdjList :: [(VertexType, [VertexType])]
exampleAdjList = [(VertexVar width, [VertexMet m1, VertexMet m4, VertexMet m2]), (VertexVar area, [VertexMet m3]), (VertexVar height, [VertexMet m2, VertexMet m1, VertexMet m5]), (VertexVar perimeter, [VertexMet m5, VertexMet m4]), (VertexMet m5, [VertexVar width]), (VertexMet m3, [VertexVar width, VertexVar height]), (VertexMet m1, [VertexVar area]), (VertexMet m2, [VertexVar perimeter]), (VertexMet m4, [VertexVar height])]

exampleGraph :: Graph VertexType
exampleGraph = stars exampleAdjList
