module NewImpl
    ( bitCombination
    , findBestSolution
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

findBestSolution :: [Constraint] -> [Constraint] -> Constraint
findBestSolution stayConstraints mustConstraints =
    let combinations = map (`bitCombination` stayConstraints) $ nCombinations $ numberOfVars $ mconcat mustConstraints
        results = zip combinations $ map (\x -> isValidSolution mustConstraints (plan (x ++ mustConstraints))) combinations
    in plan (fst (head (filter snd results)) ++ mustConstraints)

isVar :: NodeKind -> Bool
isVar (NodeVar _) = True
isVar (NodeMet _) = False

isMet :: NodeKind -> Bool
isMet = not . isVar

