{-|
Module      : WarmDrinkF
Description : Defines data structures for representing components and constraint systems with confined variables.

This module defines two main data structures, 'Component' and 'ConstraintSystem'. The 'Component' type represents a single unit in a constraint system, while the 'ConstraintSystem' type provides an encompassing structure to interrelate components. These structures form the backbone of representing warm drink preparation steps in a systematic manner.
-}

module WarmDrinkF
    ( Component (..)
    , ConstraintSystem (..)
    ) where

import           AST       (Value)
import           Data.Map  (Map)
import           HotDrinkF (Constraint)

{-|
A 'Component' represents a single entity in a constraint system for warm drinks. It consists of an identifier, a map of variable names to optional values, a list of constraints, and a list of strength values.

Each 'Component' essentially defines a unique preparation step in the creation of a warm drink, with variables detailing the parameters involved, constraints defining the dependencies or requirements, and strength values providing a measure of how strongly the constraints should be applied.
-}
data Component
  = Component
      { identifier  :: Int
        -- ^ The unique identifier for the component.
      , variables   :: Map String (Maybe Value)
        -- ^ The map of variable names to optional values. These represent the parameters of this step in the warm drink preparation process.
      , constraints :: [Constraint]
        -- ^ The list of constraints associated with this component. These can represent dependencies or requirements necessary for this step.
      , strength    :: [String]
        -- ^ The list of strength values. These represent how strongly each constraint should be applied.
      }
  deriving (Eq, Show)

{-|
A 'ConstraintSystem' represents an organized system of 'Component's for warm drinks, and intercalating constraints.

A 'ConstraintSystem' defines the full process of creating a warm drink, outlining each component (or preparation step), and the dependencies and intercalations between them. It is a higher-level structure that uses 'Component's to create a complete system.
-}
data ConstraintSystem
  = ConstraintSystem
      { components               :: [Component]
        -- ^ The list of components. Each component represents a step in the warm drink preparation process.
      , intercalatingConstraints :: [Constraint]
        -- ^ The list of intercalating constraints. These constraints define dependencies or relationships between different components.
      }
  deriving (Show)
