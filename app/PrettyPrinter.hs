module PrettyPrinter where

import qualified HotDrink as HD
import qualified Algebra.Graph.AdjacencyMap as Adj
import Data.List (intercalate)
import qualified Data.Set as Set

prettyPrintConstraint :: HD.Constraint -> String
prettyPrintConstraint (HD.Constraint methodGraphs)
    = intercalate (replicate 30 '-') (map prettyPrintMethodGraph methodGraphs)

prettyPrintMethodGraph :: HD.Method -> String
prettyPrintMethodGraph methodGraph =
    let methods = [HD.VertexMet x | HD.VertexMet x <- Adj.vertexList methodGraph]
    in intercalate "\n" (map (prettyPrintMethod methodGraph) methods)

prettyPrintMethod :: HD.Method -> HD.VertexType -> String
prettyPrintMethod methodGraph m =
    let ins = map showVertex $ Set.toList $ Adj.preSet m methodGraph
        out = map showVertex $ Set.toList $ Adj.postSet m methodGraph
    in showEdge ins (showVertex m) out

showVertex :: HD.VertexType -> String
showVertex (HD.VertexMet s) = s
showVertex (HD.VertexVar s) = s

showEdge :: [String] -> String -> [String] -> String
showEdge ins m outs = "[" <> intercalate "," ins <> "] -> " <> m <> " -> [" <> intercalate "," outs <> "]"