{-# LANGUAGE LambdaCase #-}

module Main where

import           Algebra.Graph.AdjacencyMap
import           Algebra.Graph.AdjacencyMap.Algorithm (topSort)
import           Control.Monad.State
import           Data.Bits
import           Data.Foldable                        (fold)
import           Data.Maybe                           (fromMaybe)
import           HotDrink
import           MethodParser
import qualified Whap

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

getVariables :: Constraint -> [VertexType]
getVariables (Constraint [x]) =
    map (\case VertexVar v -> VertexVar v; _ -> error "not a variable")
    $ filter (\case VertexVar _ -> True; VertexMet _ -> False)
    $ vertexList x
getVariables _ = error "not a single constraint"

concatExprsInMethodList :: [VertexType] -> [(String, Expr)]
concatExprsInMethodList = concatMap (\case VertexMet (_, es) -> es; _ -> error "not a method")

main :: IO ()
main = do
    let initialState = [("area", Just 3600.0), ("perimeter", Just 40.0), ("width", Just 10.0), ("height", Just 10.0)] :: [Variable]
        p = plan [Whap.stayArea, Whap.stayPerimeter, Whap.stayWidth, Whap.stayHeight] (mconcat [Whap.constraintA, Whap.constraintB])
        methods = methodsToEnforce p
        exprs = concatExprsInMethodList <$> methods
    case exprs of
        Just _ -> do
            (_, finalState) <- runStateT (traverse updateVariable (fromMaybe [] exprs)) initialState
            putStrLn $ "Final state: " ++ show finalState
        Nothing -> putStrLn "No methods to enforce"
    return ()
