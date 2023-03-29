{-# LANGUAGE LambdaCase #-}

module Main where

import           Algebra.Graph.AdjacencyMap
import           Algebra.Graph.AdjacencyMap.Algorithm (topSort)
import           Data.Bits
import           Data.Foldable                        (fold)
import qualified Data.Time.Clock                      as Clock
import qualified Geometry
import           HotDrink
import qualified PrettyPrinter
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

test2 :: Constraint
test2 = plan
    -- stay constraints in priority order
    [Tax.stayX2, Tax.stayX5, Tax.stayX1, Tax.stayX6, Tax.stayX9, Tax.stayX3, Tax.stayX4, Tax.stayX8, Tax.stayX7, Tax.stayX10, Tax.stayX11, Tax.stayX12, Tax.stayX13, Tax.stayX14, Tax.stayX15]
    -- must constraints
    (mconcat [Tax.constraint1, Tax.constraint2, Tax.constraint3, Tax.constraint4, Tax.constraint5, Tax.constraint6, Tax.constraint7])

planGeometry :: Maybe [VertexType]
planGeometry = methodsToEnforce $ plan [Geometry.stayrx, Geometry.stayry, Geometry.stayra, Geometry.stayrp, Geometry.stayc1r, Geometry.stayc1a, Geometry.stayc1p, Geometry.stayc2a, Geometry.stayc2p, Geometry.stayc2r] (mconcat [Geometry.c1, Geometry.c2, Geometry.c3, Geometry.c4, Geometry.c5])

-- just an example
main :: IO ()
main = do
    start2 <- Clock.getCurrentTime
    print $ length $ unConstraint (mconcat [Tax.constraint1, Tax.constraint2, Tax.constraint3, Tax.constraint4, Tax.constraint5, Tax.constraint6, Tax.constraint7])
    end2 <- Clock.getCurrentTime
    print (Clock.diffUTCTime end2 start2)

    start <- Clock.getCurrentTime
    putStrLn "Result:"
    putStrLn $ PrettyPrinter.prettyPrintConstraint test2
    end <- Clock.getCurrentTime
    print (Clock.diffUTCTime end start)



