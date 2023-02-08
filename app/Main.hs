module Main where
import           Algebra.Graph
import           Data.List     (nub)
import           Evaluator
import           GraphHelpers
import           HotDrink
import           NewImpl
import           NewResize
import           NewWhap

main :: IO ()
main = return ()

isVariable :: VertexType -> Bool
isVariable (VertexVar _) = True
isVariable _             = False

isMethod :: VertexType -> Bool
isMethod (VertexMet _) = True
isMethod _             = False

-- Get constraint from a method
-- stay constraints are not included
-- a stay constraint is a constraint where the last element is True
getConstraintFromMethod :: VertexType -> [Constraint] -> Constraint
getConstraintFromMethod (VertexMet m) constraints = head $ filter (\(x, _, s) -> m `elem` snd x && not s) constraints
getConstraintFromMethod _ _              = error "Not a method"

-- Get all constraints from a variable
getConstraintsFromVariable :: VertexType -> [Constraint] -> [Constraint]
getConstraintsFromVariable (VertexVar v) cs = [((vs, ms), p, s) | ((vs, ms), p, s) <- cs, v `elem` vs && not s]
getConstraintsFromVariable _ _              = error "Not a variable"

getStayConstraintFromVariable :: VertexType -> [Constraint] -> Constraint
getStayConstraintFromVariable (VertexVar v) cs = head $ filter (\((vs, _), _, s) -> v `elem` vs && s) cs
getStayConstraintFromVariable _ _              = error "Not a variable"

-- check if a variable is free, a variable is free if it in only in one constraint
isVariableFree :: VertexType -> [Constraint] -> Graph VertexType -> Bool
isVariableFree (VertexVar v) cs g = isVertexInGraph (VertexVar v) g && length (getConstraintsFromVariable (VertexVar v) cs) == 1
isVariableFree _ _ _        = error "Not a variable"

-- check if a method is free, a method is free if all its outbound vertices are free variables
isMethodFree :: VertexType -> [Constraint] -> Graph VertexType -> Bool
isMethodFree (VertexMet m) cs g = isVertexInGraph (VertexMet m) g && all (\x -> isVariableFree x cs g) (outboundVertices (VertexMet m) g)
isMethodFree _ _ _        = error "Not a method"

-- check if a constraint has a free method
constraintHasFreeMethod :: Constraint -> [Constraint] -> Graph VertexType -> Bool
constraintHasFreeMethod ((_, y), _, _) cs g = any ((\x -> isMethodFree x cs g) . VertexMet) y

-- get all free methods from a constraint
getFreeMethodsFromConstraint :: Constraint -> [Constraint] -> Graph VertexType -> [VertexType]
getFreeMethodsFromConstraint ((_, y), _, _) cs g = filter (\x -> isMethodFree x cs g) (map VertexMet y)

-- get arbitrary free method from a constraint
getArbitraryFreeMethodFromConstraint :: Constraint -> [Constraint] -> Graph VertexType -> VertexType
getArbitraryFreeMethodFromConstraint ((_, y), _, _) cs g = head (filter (\x -> isMethodFree x cs g) (map VertexMet y))

-- get all constraints with free methods
getConstraintsWithFreeMethods :: [Constraint] -> Graph VertexType -> [Constraint]
getConstraintsWithFreeMethods cs g = filter (\x -> constraintHasFreeMethod x cs g) cs

-- get arbitrary free method from each constraint
getArbitraryFreeMethodsFromConstraints :: [Constraint] -> Graph VertexType -> [VertexType]
getArbitraryFreeMethodsFromConstraints cs g = map (\x -> getArbitraryFreeMethodFromConstraint x cs g) (getConstraintsWithFreeMethods cs g)

removeAllMethodsExcept :: [Identifier] -> Graph VertexType -> Graph VertexType
removeAllMethodsExcept ms g = foldr removeVertex g (filter (\x -> not (isVariable x) && notElem (extractLabel x) ms) (vertexList g))

-- update value of a variable in a graph using replaceVertex
updateVariableValue :: Identifier -> Double -> Graph VertexType -> Graph VertexType
updateVariableValue ident val g =
  case lookupLabel ident g of
    Just (VertexVar (i, v)) -> replaceVertex (VertexVar (i, v)) (VertexVar (i, Just val)) g
    _                       -> error "Variable not found"

evalMethod :: Identifier -> Graph VertexType -> Graph VertexType
evalMethod i g =
  case lookupLabel i g of
    Nothing                         -> error "method does not exist"
    Just (VertexVar _)              -> error "cant evaluate a variable"
    Just (VertexMet (_, exprs))     -> foldr (\(ident, expr) g' -> updateVariableValue ident (eval g' expr) g') g exprs

solve :: [Constraint] -> Graph VertexType -> Graph VertexType
solve cs g =
  foldr evalMethod g
  $ nub
  $ map extractLabel
  $ filter isMethod
  $ topologicalSort
  $ removeAllMethodsExcept (map extractLabel (getArbitraryFreeMethodsFromConstraints cs g)) g

prioritizeConstraint :: Constraint -> Constraint
prioritizeConstraint ((vs, ms), _, s) = ((vs, ms), 0, s)

priority :: Constraint -> Int
priority (_, p, _) = p

incrementPriority :: Constraint -> Constraint
incrementPriority ((vs, ms), p, s) = ((vs, ms), p + 1, s)

-- Promote a constraint
-- Promoting a constraint mean giving it the highest priority among all constraints
-- Other constraints that initially had higher priority than the promoted constraint will decrease their priority by 1
promoteConstraint :: Constraint -> [Constraint] -> [Constraint]
promoteConstraint c = map (\c' ->
    if c' == c
      then prioritizeConstraint c'
      else if priority c' < priority c
        then incrementPriority c'
        else c')

-- Touch a variable
touchVariable :: VertexType -> [Constraint] -> [Constraint]
touchVariable (VertexVar v) cs = promoteConstraint (getStayConstraintFromVariable (VertexVar v) cs) cs
touchVariable _ _ = error "Not a variable"


-- remove a constraint from a list of constraints, along with its corresponding methods in the graph
removeConstraint :: Constraint -> Graph VertexType -> Graph VertexType
removeConstraint ((_, ms), _, _) g = foldr (removeVertex . VertexMet) g ms


