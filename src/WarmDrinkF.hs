module WarmDrinkF
    ( Component (..)
    , ConstraintSystem (..)
    ) where

import           AST       (Value)
import           Data.Map  (Map)
import           HotDrinkF (Constraint)

data Component
  = Component
      { identifier  :: Int
      , variables   :: Map String (Maybe Value)
      , constraints :: [Constraint]
      , strength    :: [String]
      }
  deriving (Eq, Show)

data ConstraintSystem -- perhaps rename to list or something???
  = ConstraintSystem
      { components               :: [Component]
      , intercalatingConstraints :: [Constraint]
      }
  deriving (Show)
