module WarmDrinkF
    ( Component (..)
    , ComponentList (..)
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

data ComponentList
  = ComponentList
      { components               :: [Component]
      , intercalatingConstraints :: [Constraint]
      }
  deriving (Show)
