module Main where
import           Algebra.Graph

main :: IO ()
main = return ()


type Identifier = String
type Variable = (Identifier, Maybe Int)
type Method = String

data VertexType = Var Variable | Met Method deriving (Show, Eq, Ord)

type Constraint = ([Variable], [Method])

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
exampleAdjList = [(Var w, [Met m1, Met m4, Met m2]), (Var a, [Met m3]), (Var h, [Met m2, Met m1, Met m5]), (Var p, [Met m5, Met m4]), (Met m5, [Var w]), (Met m3, [Var w, Var h]), (Met m1, [Var a]), (Met m2, [Var p]), (Met m4, [Var h])]

exampleGraph :: Graph VertexType
exampleGraph = stars exampleAdjList


-------- Generic functions --------

-- A function to get vertices pointing to a vertex
inboundVertices :: Ord a => a -> Graph a -> [a]
inboundVertices v g = filter (\x -> v `elem` outboundVertices x g) (vertexList g)

-- A function to get neighbourhood of a vertex
outboundVertices :: Ord a => a -> Graph a -> [a]
outboundVertices v g = [x | x <- vertexList g, x /= v, hasEdge v x g]

-- A function to get complete neighbourhood of a vertex
completeNeighbourhood :: Ord a => a -> Graph a -> [a]
completeNeighbourhood v g = [x | x <- vertexList g, x /= v, hasEdge v x g || hasEdge x v g]

-- Get all vertices with no incoming edges
getSources :: Ord a => Graph a -> [a]
getSources g = filter (\x -> null (inboundVertices x g)) (vertexList g)

-- check if vertex is part of graph
isVertexInGraph :: Ord a => a -> Graph a -> Bool
isVertexInGraph v g = v `elem` vertexList g

-------- HotDrink functions --------

isVariable :: VertexType -> Bool
isVariable (Var _) = True
isVariable _       = False

isMethod :: VertexType -> Bool
isMethod (Met _) = True
isMethod _       = False

-- Get constraint from a method
getConstraintFromMethod :: VertexType -> [Constraint] -> Constraint
getConstraintFromMethod (Met m) cs = head [c | c <- cs, m `elem` snd c]
getConstraintFromMethod _ _        = error "Not a method"

-- Get all constraints from a variable
getConstraintsFromVariable :: VertexType -> [Constraint] -> [Constraint]
getConstraintsFromVariable (Var v) cs = [c | c <- cs, v `elem` fst c]
getConstraintsFromVariable _ _        = error "Not a variable"

-- check if a variable is free, a variable is free if it in only in one constraint
isVariableFree :: VertexType -> [Constraint] -> Graph VertexType -> Bool
isVariableFree (Var v) cs g = isVertexInGraph (Var v) g && length (getConstraintsFromVariable (Var v) cs) == 1
isVariableFree _ _ _        = error "Not a variable"

-- check if a method is free, a method is free if all its outbound vertices are free variables
isMethodFree :: VertexType -> [Constraint] -> Graph VertexType -> Bool
isMethodFree (Met m) cs g = isVertexInGraph (Met m) g && all (\x -> isVariableFree x cs g) (outboundVertices (Met m) g)
isMethodFree _ _ _        = error "Not a method"

-- check if a constraint has a free method
constraintHasFreeMethod :: Constraint -> [Constraint] -> Graph VertexType -> Bool
constraintHasFreeMethod (_, y) cs g = any ((\x -> isMethodFree x cs g) . Met) y

-- get all free methods from a constraint
getFreeMethodsFromConstraint :: Constraint -> [Constraint] -> Graph VertexType -> [VertexType]
getFreeMethodsFromConstraint (_, y) cs g = filter (\x -> isMethodFree x cs g) (map Met y)

-- get arbitrary free method from a constraint
getArbitraryFreeMethodFromConstraint :: Constraint -> [Constraint] -> Graph VertexType -> VertexType
getArbitraryFreeMethodFromConstraint (_, y) cs g = head (filter (\x -> isMethodFree x cs g) (map Met y))

-- get all constraints with free methods
getConstraintsWithFreeMethods :: [Constraint] -> Graph VertexType -> [Constraint]
getConstraintsWithFreeMethods cs g = filter (\x -> constraintHasFreeMethod x cs g) cs

-- remove non free methods from a graph, but keep all variables
removeNonFreeMethods :: [Constraint] -> Graph VertexType -> Graph VertexType
removeNonFreeMethods cs g = foldr removeVertex g (filter (\x -> not (isVariable x) && not (isMethodFree x cs g)) (vertexList g))
