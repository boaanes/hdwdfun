{-# LANGUAGE LambdaCase #-}

module Algs where

import           AST
import           Algebra.Graph.AdjacencyMap
import           Algebra.Graph.AdjacencyMap.Algorithm (topSort)
import           Data.Foldable                        (fold)
import           HotDrinkF

-- check if constraint is part of another constraint (is subgraph)
partOf :: Constraint -> Constraint -> Bool
partOf c = any (`isSubgraphOf` (fold . unConstraint) c) . unConstraint

-- check if all given constraints are part of another constraint
isPartOfAllConstraints :: [Constraint] -> Constraint -> Bool
isPartOfAllConstraints constraints candidate = all (partOf candidate) constraints

plan :: [Constraint] -> Constraint -> Constraint
plan [] c = c
plan (x:xs) c =
    let cAndX@(Constraint cx) = c <> x
    in if null cx
        then plan xs c
        else plan xs cAndX

-- topsort and filter out variables to get methods to enforce
methodsToEnforce :: Constraint -> Maybe [VertexType]
methodsToEnforce (Constraint [x]) = case topSort x of
    Right es -> Just $ filter (\case VertexVar _ -> False; _ -> True) es
    Left _   -> Nothing
methodsToEnforce _ = Nothing

getLabels :: Maybe [VertexType] -> [String]
getLabels (Just vs) = map (\case VertexMet (s, _) -> s; VertexVar s -> s) vs
getLabels Nothing   = []

getVariables :: Constraint -> [VertexType]
getVariables (Constraint [x]) =
    map (\case VertexVar v -> VertexVar v; _ -> error "not a variable")
    $ filter (\case VertexVar _ -> True; VertexMet _ -> False)
    $ vertexList x
getVariables _ = error "not a single constraint"

concatExprsInMethodList :: [VertexType] -> [(String, Expr)]
concatExprsInMethodList = concatMap (\case VertexMet (_, es) -> es; _ -> error "not a method")

computePlan :: [String] -> [Constraint] -> Maybe [VertexType]
computePlan stay cs = methodsToEnforce $ plan order $ mconcat cs
  where
    order = map (\s -> Constraint [methodToGraph [] ("m" ++ s, [(s, Var s)])]) stay
