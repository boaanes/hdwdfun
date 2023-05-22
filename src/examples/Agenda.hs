module Agenda
    ( compA
    , compB
    , constraint
    , i1
    , i1Graph
    , inter
    , m1
    , m1Graph
    ) where

import           AST
import           Algebra.Graph.AdjacencyMap
import qualified Data.Map                   as Map
import           HotDrinkF
import           WarmDrinkF

m1 :: Method
m1 = ("m1", [("end", BinOp "+" (Var "start") (Var "duration"))])

vStart :: String
vStart = "start"

vDuration :: String
vDuration = "duration"

vEnd :: String
vEnd = "end"

m1Graph :: MethodGraph
m1Graph = stars [(VertexMet m1, [VertexVar vEnd]), (VertexVar vStart, [VertexMet m1]), (VertexVar vDuration, [VertexMet m1])]

constraint :: Constraint
constraint = Constraint [m1Graph]

i1 :: Method
i1 = ("i1", [("start", Var "end")])

i1Graph :: MethodGraph
i1Graph = stars [(VertexMet i1, [VertexVar vStart]), (VertexVar vEnd, [VertexMet i1])]

inter :: Constraint
inter = Constraint [i1Graph]

-- test components
compA :: Component
compA = Component 0 (Map.fromList [("start", Just (DoubleVal 1)), ("duration", Just (DoubleVal 10)), ("end", Just (DoubleVal 11))]) [constraint] ["start", "duration", "end"]

compB :: Component
compB = Component 1 (Map.fromList [("start", Just (DoubleVal 1)), ("duration", Just (DoubleVal 10)), ("end", Just (DoubleVal 11))]) [constraint] ["start", "duration", "end"]
