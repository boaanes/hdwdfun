module NewImpl
    ( bestPlan
    , bitCombination
    , isMet
    , isValidSolution
    , isVar
    , nCombinations
    , numberOfVars
    , partOf
    , plan
    ) where

import           Algebra.Graph.AdjacencyMap
import           Data.Bits
import           Data.Foldable              (find)
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

numberOfVars :: Constraint -> Int
numberOfVars = length . filter isVar . vertexList . concatMethodsInConstriant

-- return first valid solution
bestPlan :: [Constraint] -> [Constraint] -> Maybe Constraint
bestPlan stayConstraints mustConstraints =
    let combinations = map (`bitCombination` stayConstraints) $ nCombinations $ numberOfVars $ mconcat mustConstraints
        results = map (\x -> plan (x ++ mustConstraints)) combinations
    in find (isValidSolution mustConstraints) results

isVar :: NodeKind -> Bool
isVar (NodeVar _) = True
isVar (NodeMet _) = False

isMet :: NodeKind -> Bool
isMet = not . isVar

