module Main where
import           Algebra.Graph
import           HotDrink
import           MethodParser

main :: IO ()
main = return ()

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
isVariable (VertexVar _) = True
isVariable _             = False

isMethod :: VertexType -> Bool
isMethod (VertexMet _) = True
isMethod _             = False

-- Get constraint from a method
getConstraintFromMethod :: VertexType -> [Constraint] -> Constraint
getConstraintFromMethod (VertexMet m) cs = head [c | c <- cs, m `elem` snd c]
getConstraintFromMethod _ _              = error "Not a method"

-- Get all constraints from a variable
getConstraintsFromVariable :: VertexType -> [Constraint] -> [Constraint]
getConstraintsFromVariable (VertexVar v) cs = [c | c <- cs, v `elem` fst c]
getConstraintsFromVariable _ _              = error "Not a variable"

-- check if a variable is free, a variable is free if it in only in one constraint
isVariableFree :: VertexType -> [Constraint] -> Graph VertexType -> Bool
isVariableFree (VertexVar v) cs g = isVertexInGraph (VertexVar v) g && length (getConstraintsFromVariable (VertexVar v) cs) == 1
isVariableFree _ _ _        = error "Not a variable"

-- check if a method is free, a method is free if all its outbound vertices are free variables
isMethodFree :: VertexType -> [Constraint] -> Graph VertexType -> Bool
isMethodFree (VertexMet m) cs g = isVertexInGraph (VertexMet m) g && all (\x -> isVariableFree x cs g) (outboundVertices (VertexMet m) g)
isMethodFree _ _ _        = error "Not a method"

-- check if a constraint has a free method
constraintHasFreeMethod :: Constraint -> [Constraint] -> Graph VertexType -> Bool
constraintHasFreeMethod (_, y) cs g = any ((\x -> isMethodFree x cs g) . VertexMet) y

-- get all free methods from a constraint
getFreeMethodsFromConstraint :: Constraint -> [Constraint] -> Graph VertexType -> [VertexType]
getFreeMethodsFromConstraint (_, y) cs g = filter (\x -> isMethodFree x cs g) (map VertexMet y)

-- get arbitrary free method from a constraint
getArbitraryFreeMethodFromConstraint :: Constraint -> [Constraint] -> Graph VertexType -> VertexType
getArbitraryFreeMethodFromConstraint (_, y) cs g = head (filter (\x -> isMethodFree x cs g) (map VertexMet y))

-- get all constraints with free methods
getConstraintsWithFreeMethods :: [Constraint] -> Graph VertexType -> [Constraint]
getConstraintsWithFreeMethods cs g = filter (\x -> constraintHasFreeMethod x cs g) cs

-- remove non free methods from a graph, but keep all variables
removeNonFreeMethods :: [Constraint] -> Graph VertexType -> Graph VertexType
removeNonFreeMethods cs g = foldr removeVertex g (filter (\x -> not (isVariable x) && not (isMethodFree x cs g)) (vertexList g))
