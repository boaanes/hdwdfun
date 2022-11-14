module GraphHelpers where

import           Algebra.Graph
import qualified Data.List
import           HotDrink

-- extract label
extractLabel :: VertexType -> String
extractLabel (VertexVar (x, _)) = x
extractLabel (VertexMet (x, _)) = x

-- lookup label in graph
lookupLabel :: Identifier -> Graph VertexType -> Maybe VertexType
lookupLabel s g = Data.List.find (\x -> extractLabel x == s) (vertexList g)
