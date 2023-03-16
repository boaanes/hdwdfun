{-# LANGUAGE LambdaCase #-}

module Main where

import           Algebra.Graph.AdjacencyMap
import           Algebra.Graph.AdjacencyMap.Algorithm (topSort)
import           Control.Monad.IO.Class               (liftIO)
import           Data.Bits
import           Data.Foldable                        (fold)
import           HotDrink
import qualified Tax

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

f :: Constraint -> [Constraint] -> Constraint
f (Constraint []) _ = Constraint []
f acc []            = acc
f acc (x:xs)        = f (acc <> x) xs

plan' :: [Constraint] -> [Constraint] -> Int -> Maybe Constraint
plan' _ _ 0 = Nothing
plan' stayConstraints mustConstraints n =
    let combination = bitCombination n stayConstraints
        result = (f (head combination) . (++ mustConstraints)) (drop 1 combination)
    in (if isPartOfAllConstraints mustConstraints result then Just result else plan' stayConstraints mustConstraints (n-1))

plan :: [Constraint] -> [Constraint] -> Maybe Constraint
plan stayConstraints mustConstraints = plan' stayConstraints mustConstraints $ 2 ^ length stayConstraints - 1

-- topsort and filter out variables to get methods to enforce
methodsToEnforce :: Maybe Constraint -> Maybe [VertexType]
methodsToEnforce (Just (Constraint [x])) =
    case topSort x of
        Right es -> Just $ filter (\case
            VertexVar _ -> False
            _           -> True) es
        Left _   -> Nothing
methodsToEnforce _ = Nothing

-- just an example
main :: IO ()
main = do
    let result = plan' [Tax.stayX2, Tax.stayX5, Tax.stayX1, Tax.stayX6, Tax.stayX9, Tax.stayX3, Tax.stayX4, Tax.stayX8, Tax.stayX7, Tax.stayX10, Tax.stayX11, Tax.stayX12, Tax.stayX13, Tax.stayX14, Tax.stayX15] [Tax.constraint1, Tax.constraint2, Tax.constraint3, Tax.constraint4, Tax.constraint5, Tax.constraint6, Tax.constraint7] 32767
    liftIO $ print result
    liftIO $ print $ methodsToEnforce result


