module Main where
import           Algebra.Graph

main :: IO ()
main = return ()

exampleGraphAdjList :: [(Int, [Int])]
exampleGraphAdjList = [(1, [7,9,8]), (2, [6]), (3, [8, 7, 5]), (4, [5, 9]), (5, [1]), (6, [1, 3]), (7, [2]), (8, [4]), (9, [3])]

exapmleGraph :: Graph Int
exapmleGraph = Overlay (Connect (Vertex 2) (Vertex 4)) (Overlay (Connect (Vertex 1) (Vertex 2)) (Connect (Vertex 2) (Vertex 3)))

-- A function to get vertices pointing to a vertex
getInboundVertices :: Int -> Graph Int -> [Int]
getInboundVertices v g = filter (\x -> v `elem` neighbourhood x g) (vertexList g)

-- A function to get neighbourhood of a vertex
neighbourhood :: Int -> Graph Int -> [Int]
neighbourhood v g = [x | x <- vertexList g, x /= v, hasEdge v x g]

-- A function to get complete neighbourhood of a vertex
completeNeighbourhood :: Int -> Graph Int -> [Int]
completeNeighbourhood v g = [x | x <- vertexList g, x /= v, hasEdge v x g || hasEdge x v g]
