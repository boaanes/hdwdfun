{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE LambdaCase #-}
module HotDrink
    ( Constraint (..)
    , Method
    , MethodGraph
    , Variable
    , VertexType (..)
    , getLabel
    , methodUnion
    ) where

import           Algebra.Graph.AdjacencyMap
import           Algebra.Graph.AdjacencyMap.Algorithm
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
