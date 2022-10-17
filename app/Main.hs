module Main where
import           Algebra.Graph

main :: IO ()
main = return ()

data VertexType a = Variable a | Constraint a deriving (Show, Eq, Ord)

variables :: [String]
variables = ["A", "B", "C", "D"]

methods :: [String]
methods = ["E", "F", "G", "H", "I"]

constraints :: [([String], [String])]
constraints = [(["A", "B", "C"], ["F", "G"]), (["A", "C", "D"], ["E", "H", "I"])]

exampleAdjList :: [(String, [String])]
exampleAdjList = [("A", ["G","I","H"]), ("B", ["F"]), ("C", ["H", "G", "E"]), ("D", ["E", "I"]), ("E", ["A"]), ("F", ["A", "C"]), ("G", ["B"]), ("H", ["D"]), ("I", ["C"])]

exampleGraph :: Graph String
exampleGraph = stars exampleAdjList

-- A function to get vertices pointing to a vertex
inboundVertices :: Ord a =>a -> Graph a -> [a]
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

-- Get constraint from a method
getConstraintFromMethod :: Ord a => a -> [([a], [a])] -> ([a], [a])
getConstraintFromMethod v cs = head $ filter (\(_, y) -> v `elem` y) cs

-- Get all constraints from a variable
getConstraintsFromVariable :: Ord a => a -> [([a], [a])] -> [([a], [a])]
getConstraintsFromVariable v = filter (\(x, _) -> v `elem` x)

-- check if a variable is free, a variable is free if it is only in one constraint
isVariableFree :: Ord a => a -> [([a], [a])] -> Bool
isVariableFree v cs = length (getConstraintsFromVariable v cs) == 1

-- check if a method is free, a method is free if all its outbound vertices are free variables
isMethodFree :: Ord a => a -> [([a], [a])] -> Graph a -> Bool
isMethodFree m cs g = all (`isVariableFree` cs) (outboundVertices m g)
