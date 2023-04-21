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
    , eval
    , getLabel
    , methodToGraph
    , methodUnion
    ) where

import           Algebra.Graph.AdjacencyMap
import           Algebra.Graph.AdjacencyMap.Algorithm
import qualified Data.Map                             (Map, lookup)
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

eval :: Data.Map.Map String (Maybe Double) -> Expr -> Double
eval vs (BinOp "+" a b) = eval vs a + eval vs b
eval vs (BinOp "-" a b) = eval vs a - eval vs b
eval vs (BinOp "*" a b) = eval vs a * eval vs b
eval vs (BinOp "/" a b) = eval vs a / eval vs b
eval _ (BinOp {})      = error "Operator not supported"
eval vs (UnOp "sqrt" e)      = sqrt $ eval vs e
eval vs (UnOp "log" e)       = log $ eval vs e
eval vs (UnOp "exp" e)       = exp $ eval vs e
eval _ (UnOp {})       = error "Operator not supported"
eval vs (Var x)       =
    case Data.Map.lookup x vs of
        Just (Just x') -> x'
        _              -> error $ "Variable " ++ x ++ " not found"
eval _ (Lit x)       = x

type Variable = (String, Maybe Double)
type VariableState = [Variable]

methodToGraph :: [String] -> Method -> MethodGraph
methodToGraph inputs method =
    let inputVertices = map VertexVar inputs
        methodVertex = VertexMet method
        inputEdges = map (, methodVertex) inputVertices
        outputEdges = map ((methodVertex,) . VertexVar . fst) (snd method)
    in overlay (edges inputEdges) (edges outputEdges)
