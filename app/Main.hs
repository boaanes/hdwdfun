module Main where
import           Algebra.Graph
import           Data.List     (nub)
import           Evaluator
import           GraphHelpers
import           HotDrink

main :: IO ()
main = return ()

isVariable :: VertexType -> Bool
isVariable (VertexVar _) = True
isVariable _             = False

isMethod :: VertexType -> Bool
isMethod (VertexMet _) = True
isMethod _             = False

-- Get constraint from a method
getConstraintFromMethod :: VertexType -> [Constraint] -> Constraint
getConstraintFromMethod (VertexMet m) cs = head [c | c <- cs, m `elem` (snd . fst) c]
getConstraintFromMethod _ _              = error "Not a method"

-- Get all constraints from a variable
getConstraintsFromVariable :: VertexType -> [Constraint] -> [Constraint]
getConstraintsFromVariable (VertexVar v) cs = [c | c <- cs, v `elem` (fst . fst) c]
getConstraintsFromVariable _ _              = error "Not a variable"

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
constraintHasFreeMethod ((_, y), _) cs g = any ((\x -> isMethodFree x cs g) . VertexMet) y

-- get all free methods from a constraint
getFreeMethodsFromConstraint :: Constraint -> [Constraint] -> Graph VertexType -> [VertexType]
getFreeMethodsFromConstraint ((_, y), _) cs g = filter (\x -> isMethodFree x cs g) (map VertexMet y)

-- get arbitrary free method from a constraint
getArbitraryFreeMethodFromConstraint :: Constraint -> [Constraint] -> Graph VertexType -> VertexType
getArbitraryFreeMethodFromConstraint ((_, y), _) cs g = head (filter (\x -> isMethodFree x cs g) (map VertexMet y))

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

-- Promote a constraint
-- Promoting a constraint mean giving it the highest priority among all constraints
-- Other constraints that initially had higher priority than the promoted constraint will decrease their priority by 1
promoteConstraint :: Constraint -> [Constraint] -> [Constraint]
promoteConstraint c = map (\c' ->
    if c' == c
      then (fst c', 0)
      else if snd c' < snd c
        then (fst c', snd c' + 1)
        else c')

-- Touch a variable
-- touching a variable means promoting its constraint as long as the variable is free
touchVariable :: VertexType -> [Constraint] -> Graph VertexType -> [Constraint]
touchVariable (VertexVar v) cs g =
  if isVariableFree (VertexVar v) cs g
    then promoteConstraint (head (getConstraintsFromVariable (VertexVar v) cs)) cs
    else cs
touchVariable _ _ _ = error "Not a variable"

