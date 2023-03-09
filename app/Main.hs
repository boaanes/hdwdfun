{-# LANGUAGE LambdaCase #-}

module Main where

import           Algebra.Graph.AdjacencyMap
import           Algebra.Graph.AdjacencyMap.Algorithm (topSort)
import           Data.Bits
import           Data.Foldable                        (find, fold)
import           HotDrink

main :: IO ()
main = return ()

nCombinations :: Int -> [Int]
nCombinations n = reverse [0..2^n - 1]

-- get all combinations of bits given n
bitCombination :: Int -> [a] -> [a]
bitCombination _ []     = []
bitCombination n (x:xs) =
  if testBit n (length xs)
    then x : bitCombination n xs
    else bitCombination n xs

-- check if constraint is part of another constraint (is subgraph)
partOf :: Constraint -> Constraint -> Bool
partOf c = any (`isSubgraphOf` (fold . unConstraint) c) . unConstraint

-- check if all given constraints are part of another constraint
isPartOfAllConstraints :: [Constraint] -> Constraint -> Bool
isPartOfAllConstraints constraints candidate = all (partOf candidate) constraints

-- return first (also best) valid solution
plan :: [Constraint] -> [Constraint] -> Maybe Constraint
plan stayConstraints mustConstraints =
    let combinations = map (`bitCombination` stayConstraints) $ nCombinations $ length stayConstraints
        results = map (fold . (++ mustConstraints)) combinations
    in find (isPartOfAllConstraints mustConstraints) results

-- topsort and filter out variables to get methods to enforce
methodsToEnforce :: Maybe Constraint -> Maybe [NodeKind]
methodsToEnforce (Just (Constraint [x])) =
    case topSort x of
        Right es -> Just $ filter (\case
            NodeVar _ -> False
            _         -> True) es
        Left _   -> Nothing
methodsToEnforce _ = Nothing

