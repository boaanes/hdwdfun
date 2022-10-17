module Main where
import           Algebra.Graph

main :: IO ()
main = return ()

data VertexType a = Variable a | Method a deriving (Show, Eq, Ord)

type Constraint a = ([VertexType a], [VertexType a])

constraintA :: Constraint String
constraintA = ([Variable "A", Variable "B", Variable "C"], [Method "F", Method "G"])

constraintB :: Constraint String
constraintB = ([Variable "A", Variable "C", Variable "D"], [Method "E", Method "H", Method "I"])

constraints :: [Constraint String]
constraints = [constraintA, constraintB]

exampleAdjList :: [(VertexType String, [VertexType String])]
exampleAdjList = [(Variable "A", [Method "G", Method "I", Method "H"]), (Variable "B", [Method "F"]), (Variable "C", [Method "H", Method "G", Method "E"]), (Variable "D", [Method "E", Method "I"]), (Method "E", [Variable "A"]), (Method "F", [Variable "A", Variable "C"]), (Method "G", [Variable "B"]), (Method "H", [Variable "D"]), (Method "I", [Variable "C"])]

exampleGraph :: Graph (VertexType String)
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

isVariable :: VertexType a -> Bool
isVariable (Variable _) = True
isVariable _            = False

isMethod :: VertexType a -> Bool
isMethod (Method _) = True
isMethod _          = False

-- Get constraint from a method
getConstraintFromMethod :: (Ord a) => VertexType a -> [Constraint a] -> Constraint a
getConstraintFromMethod v cs = head $ filter (\(_, y) -> v `elem` y) cs

-- Get all constraints from a variable
getConstraintsFromVariable :: Ord a => VertexType a -> [Constraint a] -> [Constraint a]
getConstraintsFromVariable v = filter (\(x, _) -> v `elem` x)

-- check if a variable is free, a variable is free if it is only in one constraint
isVariableFree :: Ord a => VertexType a -> [Constraint a] -> Graph (VertexType a) -> Bool
isVariableFree v cs g = isVertexInGraph v g && length (getConstraintsFromVariable v cs) == 1

-- check if a method is free, a method is free if all its outbound vertices are free variables
isMethodFree :: Ord a => VertexType a -> [Constraint a] -> Graph (VertexType a) -> Bool
isMethodFree v cs g = isVertexInGraph v g && all (\x -> isVariableFree x cs g) (outboundVertices v g)

-- check if a constraint has a free method
constraintHasFreeMethod :: Ord a => Constraint a -> [Constraint a] -> Graph (VertexType a) -> Bool
constraintHasFreeMethod (_, y) cs g = any (\x -> isMethodFree x cs g) y

-- get all free methods from a constraint
getFreeMethodsFromConstraint :: Ord a => Constraint a -> [Constraint a] -> Graph (VertexType a) -> [VertexType a]
getFreeMethodsFromConstraint (_, y) cs g = filter (\x -> isMethodFree x cs g) y

-- get arbitrary free method from a constraint
getArbitraryFreeMethodFromConstraint :: Ord a => Constraint a -> [Constraint a] -> Graph (VertexType a) -> VertexType a
getArbitraryFreeMethodFromConstraint c cs g = head $ getFreeMethodsFromConstraint c cs g

-- get all constraints with free methods
getConstraintsWithFreeMethods :: Ord a => [Constraint a] -> Graph (VertexType a) -> [Constraint a]
getConstraintsWithFreeMethods cs g = filter (\x -> constraintHasFreeMethod x cs g) cs

-- Remove non free methods from a graph, but keep all variables
removeNonFreeMethods :: Ord a => Graph (VertexType a) -> [Constraint a] -> Graph (VertexType a)
removeNonFreeMethods g cs = foldr removeVertex g (filter (\x -> not (isMethodFree x cs g) && not (isVariable x)) (vertexList g))
