module NewImpl
    ( bitCombination
    , isMet
    , isVar
    , nCombinations
    , plan
    ) where

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

isVar :: NodeKind -> Bool
isVar (NodeVar _) = True
isVar (NodeMet _) = False

isMet :: NodeKind -> Bool
isMet = not . isVar

