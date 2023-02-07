{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE LambdaCase #-}
module NewImpl
    ( Method
    , NodeKind (..)
    , constraintA
    , constraintB
    , m1
    , m2
    , m3
    , m4
    , m5
    , mArea
    , mHeight
    , mPerimeter
    , mWidth
    , methodUnion
    , plan
    , stayArea
    , stayHeight
    , stayPerimeter
    , stayWidth
    ) where

import           Algebra.Graph.AdjacencyMap
import           Algebra.Graph.AdjacencyMap.Algorithm (isAcyclic)
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

plan :: [Constraint] -> Constraint
plan = foldl (\a b -> if a <> b == Constraint [] then a else a <> b) mempty

m1 :: Method
m1 = stars [(NodeMet "m1", [NodeVar "area"]), (NodeVar "width", [NodeMet "m1"]), (NodeVar "height", [NodeMet "m1"])]

m2 :: Method
m2 = stars [(NodeMet "m2", [NodeVar "perimeter"]), (NodeVar "width", [NodeMet "m2"]), (NodeVar "height", [NodeMet "m2"])]

m3 :: Method
m3 = stars [(NodeMet "m3", [NodeVar "height", NodeVar "width"]), (NodeVar "area", [NodeMet "m3"])]

m4 :: Method
m4 = stars [(NodeMet "m4", [NodeVar "height"]), (NodeVar "perimeter", [NodeMet "m4"]), (NodeVar "width", [NodeMet "m4"])]

m5 :: Method
m5 = stars [(NodeMet "m5", [NodeVar "width"]), (NodeVar "perimeter", [NodeMet "m5"]), (NodeVar "height", [NodeMet "m5"])]

constraintA :: Constraint
constraintA = Constraint [m1, m3]

constraintB :: Constraint
constraintB = Constraint [m2, m4, m5]

mArea :: Method
mArea = stars [(NodeMet "mArea", [NodeVar "area"])]

mPerimeter :: Method
mPerimeter = stars [(NodeMet "mPerimeter", [NodeVar "perimeter"])]

mWidth :: Method
mWidth = stars [(NodeMet "mWidth", [NodeVar "width"])]

mHeight :: Method
mHeight = stars [(NodeMet "mHeight", [NodeVar "height"])]

stayArea :: Constraint
stayArea = Constraint [mArea]

stayPerimeter :: Constraint
stayPerimeter = Constraint [mPerimeter]

stayWidth :: Constraint
stayWidth = Constraint [mWidth]

stayHeight :: Constraint
stayHeight = Constraint [mHeight]

