{-# LANGUAGE LambdaCase #-}
module NewImpl
    ( bestPlan
    , bitCombination
    , getLabel
    , isValidSolution
    , methodsToEnforce
    , nCombinations
    , partOf
    , plan
    ) where

import           Algebra.Graph.AdjacencyMap
import           Algebra.Graph.AdjacencyMap.Algorithm (topSort)
import           Data.Bits
import           Data.Foldable                        (find)
import           NewDatastruct

plan :: [Constraint] -> Constraint
plan = foldl (\a b -> if a <> b == Constraint [] then a else a <> b) mempty

nCombinations :: Int -> [Int]
nCombinations n = reverse [0..2^n - 1]

bitCombination :: Int -> [a] -> [a]
bitCombination _ []     = []
bitCombination n (x:xs) =
  if testBit n (length xs)
    then x : bitCombination n xs
    else bitCombination n xs

partOf :: Constraint -> Constraint -> Bool
partOf (Constraint methods) candidate = any (`isSubgraphOf` concatMethodsInConstriant candidate) methods

isValidSolution :: [Constraint] -> Constraint -> Bool
isValidSolution constraints candidate = all (`partOf` candidate) constraints

-- return first valid solution
bestPlan :: [Constraint] -> [Constraint] -> Maybe Constraint
bestPlan stayConstraints mustConstraints =
    let combinations = map (`bitCombination` stayConstraints) $ nCombinations $ length stayConstraints
        results = map (\x -> plan (x ++ mustConstraints)) combinations
    in find (isValidSolution mustConstraints) results


methodsToEnforce :: Maybe Constraint -> Maybe [NodeKind]
methodsToEnforce (Just (Constraint [x])) =
    case topSort x of
        Right es -> Just $ filter (\case
            NodeVar _ -> False
            _         -> True) es
        Left _   -> Nothing
methodsToEnforce _ = Nothing

getLabel :: NodeKind -> String
getLabel (NodeVar x) = x
getLabel (NodeMet x) = x
