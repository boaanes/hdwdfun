{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TupleSections #-}

{-|
Module      : HotDrinkF
Description : Defines our implementation of a multiway dataflow constraint system.

This module contains the data structures (most importantly Constraint) and its monoid instance
used by the planner.
-}

module HotDrinkF
    ( Constraint (..)
    , Method
    , MethodGraph
    , VertexType (..)
    , eval
    , getLabel
    , methodToGraph
    , methodUnion
    ) where

import           AST
import           Algebra.Graph.AdjacencyMap
import           Algebra.Graph.AdjacencyMap.Algorithm
import           Control.Monad                        (join)
import qualified Data.Map                             (Map, lookup)
import           Data.Maybe                           (catMaybes)
import           GraphHelpers

--- Types and Data Structures ---

-- | Type alias for representing a method as a tuple of its name and a list of parameter expressions
type Method = (String, [(String, Expr)])

-- | Type to represent vertices in the method graph which can be either a variable or a method
data VertexType
  = VertexVar String
  -- ^ Represents a variable vertex
  | VertexMet Method
  -- ^ Represents a method vertex
  deriving (Eq, Ord, Show)

-- | Type alias for representing the method graph as an adjacency map of vertex types
type MethodGraph = AdjacencyMap VertexType

-- | Newtype wrapper for a list of method graphs to represent a constraint
newtype Constraint
  = Constraint { unConstraint :: [MethodGraph] }
  -- ^ Unwraps a 'Constraint' to expose its list of method graphs
  deriving (Eq, Show)

-- | Function to combine two method graphs by overlaying them and checking if the resulting graph is acyclic and if the incoming degree of all variable vertices is at most 1
methodUnion :: MethodGraph -- ^ First method graph to combine
            -> MethodGraph -- ^ Second method graph to combine
            -> Maybe MethodGraph -- ^ Combined method graph wrapped in a 'Maybe' type
methodUnion g1 g2 =
  let g = overlay g1 g2 -- Overlay the two method graphs
      maxInDegree1 = all ((<= 1) . length . (`inboundVertices` g)) -- Check if the incoming degree of all variable vertices is at most 1
        (filter (\case
          (VertexVar _) -> True
          (VertexMet _) -> False) (vertexList g))
  in if maxInDegree1 && isAcyclic g -- Check if the graph is acyclic and if the incoming degree of all variable vertices is at most 1
    then Just g -- Return the combined graph wrapped in a 'Just' type if it satisfies the conditions
    else Nothing -- Return 'Nothing' if the combined graph is cyclic or if any variable vertex has an incoming degree greater than 1

-- | 'Semigroup' instance for 'Constraint' to combine two constraints by combining their lists of method graphs using 'methodUnion'
instance Semigroup Constraint where
  -- | Function to combine two constraints by combining their lists of method graphs using 'methodUnion'
  Constraint as <> Constraint bs = Constraint $ catMaybes [methodUnion a b | a <- as, b <- bs]

-- | 'Monoid' instance for 'Constraint' with 'mempty' as a 'Constraint' with an empty list of method graphs and 'mappend' as 'Semigroup' instance's '<>'
instance Monoid Constraint where
  mempty = Constraint [empty] -- ^ Empty 'Constraint' with an empty method graph
  mappend = (<>) -- ^ Function to combine two constraints using 'Semigroup' instance's '<>'```


--- AST Evaluation ---

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

--- Helper Functions ---

methodToGraph :: [String] -> Method -> MethodGraph
methodToGraph inputs method =
    let inputVertices = map VertexVar inputs
        methodVertex = VertexMet method
        inputEdges = map (, methodVertex) inputVertices
        outputEdges = map ((methodVertex,) . VertexVar . fst) (snd method)
    in overlay (edges inputEdges) (edges outputEdges)

getLabel :: VertexType -> String
getLabel (VertexVar s)      = s
getLabel (VertexMet (s, _)) = s
