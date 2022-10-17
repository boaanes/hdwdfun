module Main where
import           Algebra.Graph

main :: IO ()
main = return ()

exampleGraphAdjList :: [(Int, [Int])]
exampleGraphAdjList = [(1, [7,9,8]), (2, [6]), (3, [8, 7, 5]), (4, [5, 9]), (5, [1]), (6, [1, 3]), (7, [2]), (8, [4]), (9, [3])]

exampleGraph :: Graph Int
exampleGraph = stars exampleGraphAdjList

-- A function to get vertices pointing to a vertex
inboundVertices :: Ord a =>a -> Graph a -> [a]
inboundVertices v g = filter (\x -> v `elem` outboundVertices x g) (vertexList g)

-- A function to get neighbourhood of a vertex
outboundVertices :: Ord a => a -> Graph a -> [a]
outboundVertices v g = [x | x <- vertexList g, x /= v, hasEdge v x g]

-- A function to get complete neighbourhood of a vertex
completeNeighbourhood :: Ord a => a -> Graph a -> [a]
completeNeighbourhood v g = [x | x <- vertexList g, x /= v, hasEdge v x g || hasEdge x v g]

