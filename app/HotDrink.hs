{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE LambdaCase #-}
module HotDrink
    ( Constraint (..)
    , Method
    , NodeKind (..)
    , methodUnion
    ) where

import           Algebra.Graph.AdjacencyMap
import           Algebra.Graph.AdjacencyMap.Algorithm
import           Data.Maybe                           (catMaybes)
import           GraphHelpers

data NodeKind
  = NodeVar String
  | NodeMet String
  deriving (Eq, Ord, Show)

type Method = AdjacencyMap NodeKind

newtype Constraint
  = Constraint { unConstraint :: [Method] }
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

