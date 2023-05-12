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
import           Control.Monad                        (join)
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

eval ::  Expr -> Data.Map.Map String (Maybe Value) -> Maybe Value
eval (BinOp op e1 e2) env = do
  v1 <- eval e1 env
  v2 <- eval e2 env
  case op of
    "+"  -> liftBinOp (+) v1 v2
    "-"  -> liftBinOp (-) v1 v2
    "*"  -> liftBinOp (*) v1 v2
    "/"  -> liftBinOp (/) v1 v2
    "==" -> liftBoolBinOp (==) v1 v2
    "!=" -> liftBoolBinOp (/=) v1 v2
    _    -> Nothing
eval (UnOp op e) env = do
  v <- eval e env
  case op of
    "!"    -> liftBoolUnOp not v
    "sqrt" -> liftDoubleUnOp sqrt v
    "log"  -> liftDoubleUnOp log v
    _      -> Nothing
eval (Var name) env = do join $ Data.Map.lookup name env
eval (Lit v) _ = Just v

liftBinOp :: (Double -> Double -> Double) -> Value -> Value -> Maybe Value
liftBinOp f (DoubleVal a) (DoubleVal b) = Just (DoubleVal (f a b))
liftBinOp _ _ _                         = Nothing

liftBoolBinOp :: (Bool -> Bool -> Bool) -> Value -> Value -> Maybe Value
liftBoolBinOp f (BoolVal a) (BoolVal b) = Just (BoolVal (f a b))
liftBoolBinOp _ _ _                     = Nothing

liftBoolUnOp :: (Bool -> Bool) -> Value -> Maybe Value
liftBoolUnOp f (BoolVal a) = Just (BoolVal (f a))
liftBoolUnOp _ _           = Nothing

liftDoubleUnOp :: (Double -> Double) -> Value -> Maybe Value
liftDoubleUnOp f (DoubleVal a) = Just (DoubleVal (f a))
liftDoubleUnOp _ _             = Nothing

type Variable = (String, Maybe Double)
type VariableState = [Variable]

methodToGraph :: [String] -> Method -> MethodGraph
methodToGraph inputs method =
    let inputVertices = map VertexVar inputs
        methodVertex = VertexMet method
        inputEdges = map (, methodVertex) inputVertices
        outputEdges = map ((methodVertex,) . VertexVar . fst) (snd method)
    in overlay (edges inputEdges) (edges outputEdges)
