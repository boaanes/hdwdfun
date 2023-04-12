{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TupleSections #-}
module HotDrink
    ( Constraint (..)
    , Method
    , MethodGraph
    , Variable
    , VertexType (..)
    , addVariable
    , getLabel
    , getVariable
    , getVariables
    , methodUnion
    , removeVariable
    , setVariable
    ) where

import           Algebra.Graph.AdjacencyMap
import           Algebra.Graph.AdjacencyMap.Algorithm
import           Control.Monad.State
import           Data.Maybe                           (catMaybes)
import           GraphHelpers
import           MethodParser

type Variable = (String, Maybe Double)
type Method = (String, [(String, Expr)])

data VertexType
  = VertexVar Variable
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
getLabel (VertexVar (s, _)) = s
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

getVariable :: String -> VariableMonad (Maybe Variable)
getVariable s = do
    gets (fmap (s,) . lookup s) -- some magic right here

setVariable :: String -> Double -> VariableMonad ()
setVariable s d = do
    vs <- get
    let vs' = filter (\(s', _) -> s /= s') vs
    put ((s, Just d) : vs')
