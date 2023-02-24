module GraphHelpers
    ( inboundVertices
    , outboundVertices
    ) where

import           Algebra.Graph.AdjacencyMap

-------- Generic functions --------

-- A function to get vertices pointing to a vertex
inboundVertices :: Ord a => a -> AdjacencyMap a -> [a]
inboundVertices v g = filter (\x -> v `elem` outboundVertices x g) (vertexList g)

-- A function to get neighbourhood of a vertex
outboundVertices :: Ord a => a -> AdjacencyMap a -> [a]
outboundVertices v g = [x | x <- vertexList g, x /= v, hasEdge v x g]
