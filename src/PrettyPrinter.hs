{-# LANGUAGE LambdaCase #-}
module PrettyPrinter
    ( prettyPrintConstraint
    , prettyPrintMethodGraph
    , prettyPrintMethod
    , showComponent
    , showVariablesOfComponent
    , showConstraintsOfComponent
    , showStrengthOfComponent
    , showPlanOfComponent
    )
    where

import qualified Algebra.Graph.AdjacencyMap as Adj
import           Data.List                  (intercalate)
import qualified Data.Set                   as Set
import           HotDrinkF
import           WarmDrinkF
import qualified Data.Map                   as Map
import           Algs                       (getLabels, computePlan)

prettyPrintConstraint :: Constraint -> String
prettyPrintConstraint (Constraint methodGraphs)
    = intercalate "\n" (map prettyPrintMethodGraph methodGraphs)

prettyPrintMethodGraph :: MethodGraph -> String
prettyPrintMethodGraph methodGraph =
    let methods = [VertexMet x | VertexMet x <- Adj.vertexList methodGraph]
    in intercalate "\n" (map (prettyPrintMethod methodGraph) methods)

prettyPrintMethod :: MethodGraph -> VertexType -> String
prettyPrintMethod methodGraph m =
    let ins = map showVertex $ Set.toList $ Adj.preSet m methodGraph
        out = map showVertex $ Set.toList $ Adj.postSet m methodGraph
    in showEdge ins (showVertex m) out

showVertex :: VertexType -> String
showVertex (VertexMet (s, _)) = s
showVertex (VertexVar s)      = s

showEdge :: [String] -> String -> [String] -> String
showEdge ins m outs = "[" <> intercalate "," ins <> "] -> " <> m <> " -> [" <> intercalate "," outs <> "]"

showComponent :: Component -> String
showComponent c = "Component " ++ show (identifier c) ++ ": " ++ "\n" ++ showVariablesOfComponent c

showVariablesOfComponent :: Component -> String
showVariablesOfComponent c = intercalate "\n" (map (\case (k, Nothing) -> k ++ " = Nothing"; (k, Just v) -> k ++ " = " ++ show v) (Map.toList (variables c)))

showConstraintsOfComponent :: Component -> String
showConstraintsOfComponent c = intercalate "\n" (map (\c' -> prettyPrintConstraint c' ++ "\n" ++ replicate 80 '-') (constraints c))

showStrengthOfComponent :: Component -> String
showStrengthOfComponent comp = show (strength comp)

showPlanOfComponent :: Component -> String
showPlanOfComponent c = maybe "No plan found" (\m -> intercalate " -> " (getLabels $ Just m)) (computePlan (strength c) (constraints c))
