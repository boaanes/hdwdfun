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

bitCombination :: Int -> [a] -> [a]
bitCombination _ []     = []
bitCombination n (x:xs) =
  if testBit n (length xs)
    then x : bitCombination n xs
    else bitCombination n xs

partOf :: Constraint -> Constraint -> Bool
partOf c = any (`isSubgraphOf` concatMethodsInConstraint c) . unConstraint

isPartOfAllConstraints :: [Constraint] -> Constraint -> Bool
isPartOfAllConstraints constraints candidate = all (partOf candidate) constraints

-- return first (also best) valid solution
plan :: [Constraint] -> [Constraint] -> Maybe Constraint
plan stayConstraints mustConstraints =
    let combinations = map (`bitCombination` stayConstraints) $ nCombinations $ length stayConstraints
        results = map (fold . (++ mustConstraints)) combinations
    in find (isPartOfAllConstraints mustConstraints) results


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
