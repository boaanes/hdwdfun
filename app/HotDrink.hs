{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TupleSections #-}

module HotDrink
    ( Constraint (..)
    , Method
    , MethodGraph
    , Variable
    , VariableState
    , VertexType (..)
    , getLabel
    , methodToGraph
    , methodUnion
    , printVariable
    , updateVariable
    ) where

import           Algebra.Graph.AdjacencyMap
import           Algebra.Graph.AdjacencyMap.Algorithm
import           Control.Monad.State
import           Data.Maybe                           (catMaybes)
import           GraphHelpers
import           MethodParser

type Method = (String, [(String, Expr)])

data VertexType
  = VertexVar String
  | VertexMet Method
  deriving (Eq, Ord, Show)

type MethodGraph = AdjacencyMap VertexType

newtype Constraint
  = Constraint { unConstraint :: [MethodGraph] }
  deriving (Eq, Show)

methodUnion :: MethodGraph -> MethodGraph -> Maybe MethodGraph
methodUnion g1 g2 =
  let g = overlay g1 g2
      maxInDegree1 = all ((<= 1) . length . (`inboundVertices` g))
        (filter (\case
          (VertexVar _) -> True
          (VertexMet _) -> False) (vertexList g))
  in if maxInDegree1 && isAcyclic g
    then Just g
    else Nothing

instance Semigroup Constraint where
  Constraint as <> Constraint bs = Constraint $ catMaybes [methodUnion a b | a <- as, b <- bs]

instance Monoid Constraint where
  mempty = Constraint [empty]
  mappend = (<>)

getLabel :: VertexType -> String
getLabel (VertexVar s)      = s
getLabel (VertexMet (s, _)) = s

eval :: [Variable] -> Expr -> Double
eval vs (BinOp "+" a b) = eval vs a + eval vs b
eval vs (BinOp "-" a b) = eval vs a - eval vs b
eval vs (BinOp "*" a b) = eval vs a * eval vs b
eval vs (BinOp "/" a b) = eval vs a / eval vs b
eval _ (BinOp {})      = error "Operator not supported"
eval vs (Sqrt e)      = sqrt $ eval vs e
eval vs (Var x)       =
    case lookup x vs of
        Just (Just v) -> v
        _             -> error $ "Variable " <> x <> " not found"
eval _ (Lit x)       = x

type Variable = (String, Maybe Double)
type VariableState = [Variable]

updateVariable :: (String, Expr) -> StateT VariableState IO ()
updateVariable (name, e) = do
    vars <- get
    let newVal = eval vars e
    put $ (name, Just newVal) : filter (\(s, _) -> s /= name) vars

printVariable :: String -> StateT VariableState IO ()
printVariable name = do
    vars <- get
    let val = lookup name vars
    liftIO $ putStrLn $ "Variable " ++ name ++ " = " ++ show val

methodToGraph :: [String] -> Method -> MethodGraph
methodToGraph inputs method =
    let inputVertices = map VertexVar inputs
        methodVertex = VertexMet method
        inputEdges = map (, methodVertex) inputVertices
        outputEdges = map ((methodVertex,) . VertexVar . fst) (snd method)
    in overlay (edges inputEdges) (edges outputEdges)
