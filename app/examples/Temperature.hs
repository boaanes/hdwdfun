module Temperature
    ( c
    , constraints
    , exampleGraph
    , exampleGraph2
    , f
    , m1
    , m2
    ) where

import           Algebra.Graph
import           HotDrink
import           MethodParser

c :: Variable
c = ("c", Just (-40))

f :: Variable
f = ("f", Just (-40))

m1 :: Method
m1 = ("m1", [("c", Bin "*" (Bin "-" (Var "f") (Lit 32)) (Bin "/" (Lit 5) (Lit 9)))])

m2 :: Method
m2 = ("m2", [("f", Bin "+" (Bin "*" (Var "c") (Bin "/" (Lit 9) (Lit 5))) (Lit 32))])

f2 :: Variable
f2 = ("f", Just 100)

constraintA :: Constraint
constraintA = (([c, f], [m1, m1]), 1)

constraints :: [Constraint]
constraints = [constraintA]

exampleAdjList :: [(VertexType, [VertexType])]
exampleAdjList = [(VertexVar c, [VertexMet m2]), (VertexVar f, [VertexMet m1]), (VertexMet m1, [VertexVar c]), (VertexMet m2, [VertexVar f])]

exampleAdjList2 :: [(VertexType, [VertexType])]
exampleAdjList2 = [(VertexVar c, [VertexMet m2]), (VertexVar f2, [VertexMet m1]), (VertexMet m1, [VertexVar c]), (VertexMet m2, [VertexVar f2])]

exampleGraph :: Graph VertexType
exampleGraph = stars exampleAdjList

exampleGraph2 :: Graph VertexType
exampleGraph2 = stars exampleAdjList2
