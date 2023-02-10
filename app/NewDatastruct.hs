{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE LambdaCase #-}
module NewDatastruct
    ( Constraint (..)
    , Method
    , NodeKind (..)
    , concatMethodsInConstriant
    , methodUnion
    ) where

import           Algebra.Graph.AdjacencyMap
import           Algebra.Graph.AdjacencyMap.Algorithm
import           Data.Maybe                           (catMaybes)
import           NewGH

data NodeKind
  = NodeVar String
  | NodeMet String
  deriving (Eq, Ord, Show)

type Method = AdjacencyMap NodeKind

data Constraint
  = Constraint [Method]
  deriving (Eq, Show)

methodUnion :: Method -> Method -> Maybe Method
methodUnion g1 g2 =
  let g = overlay g1 g2
  in if all ((<= 1) . length . (`inboundVertices` g))
    (filter (\case
      (NodeVar _) -> True
      (NodeMet _) -> False) (vertexList g)) &&
      isAcyclic g
    then Just g
    else Nothing

instance Semigroup Constraint where
  Constraint as <> Constraint bs = Constraint $ catMaybes [methodUnion a b | a <- as, b <- bs]

instance Monoid Constraint where
  mempty = Constraint [empty]
  mappend = (<>)

concatMethodsInConstriant :: Constraint -> Method
concatMethodsInConstriant (Constraint [])     = empty
concatMethodsInConstriant (Constraint (a:as)) = a <> concatMethodsInConstriant (Constraint as)

