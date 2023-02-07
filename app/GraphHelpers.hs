module GraphHelpers
    ( completeNeighbourhood
    , extractLabel
    , getSources
    , hasCycle
    , inboundVertices
    , isVertexInGraph
    , lookupLabel
    , outboundVertices
    , topologicalSort
    ) where

import           Algebra.Graph
import qualified Data.List
import           HotDrink

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

-- Topological sort of a graph
topologicalSort :: Ord a => Graph a -> [a]
topologicalSort g = topologicalSort' g (getSources g) []

topologicalSort' :: Ord a => Graph a -> [a] -> [a] -> [a]
topologicalSort' _ [] sorted = sorted
topologicalSort' g (v:vs) sorted = topologicalSort' g (vs ++ outboundVertices v g) (sorted ++ [v])

-- detect cycle in graph by using dfs
hasCycle :: Ord a => Graph a -> Bool
hasCycle g = hasCycle' g (getSources g) []

hasCycle' :: Ord a => Graph a -> [a] -> [a] -> Bool
hasCycle' _ [] _ = False
hasCycle' g (v:vs) visited
  | v `elem` visited = True
  | otherwise = hasCycle' g (vs ++ outboundVertices v g) (visited ++ [v])

-------- HotDrink specific functions --------
extractLabel :: VertexType -> String
extractLabel (VertexVar (x, _)) = x
extractLabel (VertexMet (x, _)) = x

lookupLabel :: Identifier -> Graph VertexType -> Maybe VertexType
lookupLabel s g = Data.List.find (\x -> extractLabel x == s) (vertexList g)
