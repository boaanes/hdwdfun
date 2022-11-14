module Main where
import           Algebra.Graph
import           Data.List     (nub)
import           Evaluator
import           GraphHelpers
import           HotDrink

main :: IO ()
main = return ()

-------- Generic functions --------

-- A function to get vertices pointing to a vertex
inboundVertices :: Ord a => a -> Graph a -> [a]
inboundVertices v g = filter (\x -> v `elem` outboundVertices x g) (vertexList g)

-- A function to get neighbourhood of a vertex
outboundVertices :: Ord a => a -> Graph a -> [a]
outboundVertices v g = [x | x <- vertexList g, x /= v, hasEdge v x g]

-- A function to get complete neighbourhood of a vertex
completeNeighbourhood :: Ord a => a -> Graph a -> [a]
completeNeighbourhood v g = [x | x <- vertexList g, x /= v, hasEdge v x g || hasEdge x v g]

-- Get all vertices with no incoming edges
getSources :: Ord a => Graph a -> [a]
getSources g = filter (\x -> null (inboundVertices x g)) (vertexList g)

-- check if vertex is part of graph
isVertexInGraph :: Ord a => a -> Graph a -> Bool
isVertexInGraph v g = v `elem` vertexList g

-- Topological sort of a graph
topologicalSort :: Ord a => Graph a -> [a]
topologicalSort g = topologicalSort' g (getSources g) []

topologicalSort' :: Ord a => Graph a -> [a] -> [a] -> [a]
topologicalSort' _ [] sorted = sorted
topologicalSort' g (v:vs) sorted = topologicalSort' g (vs ++ outboundVertices v g) (sorted ++ [v])

-------- HotDrink functions --------

isVariable :: VertexType -> Bool
isVariable (VertexVar _) = True
isVariable _             = False

isMethod :: VertexType -> Bool
isMethod (VertexMet _) = True
isMethod _             = False

-- Get constraint from a method
getConstraintFromMethod :: VertexType -> [Constraint] -> Constraint
getConstraintFromMethod (VertexMet m) cs = head [c | c <- cs, m `elem` snd c]
getConstraintFromMethod _ _              = error "Not a method"

-- Get all constraints from a variable
getConstraintsFromVariable :: VertexType -> [Constraint] -> [Constraint]
getConstraintsFromVariable (VertexVar v) cs = [c | c <- cs, v `elem` fst c]
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
constraintHasFreeMethod (_, y) cs g = any ((\x -> isMethodFree x cs g) . VertexMet) y

-- get all free methods from a constraint
getFreeMethodsFromConstraint :: Constraint -> [Constraint] -> Graph VertexType -> [VertexType]
getFreeMethodsFromConstraint (_, y) cs g = filter (\x -> isMethodFree x cs g) (map VertexMet y)

-- get arbitrary free method from a constraint
getArbitraryFreeMethodFromConstraint :: Constraint -> [Constraint] -> Graph VertexType -> VertexType
getArbitraryFreeMethodFromConstraint (_, y) cs g = head (filter (\x -> isMethodFree x cs g) (map VertexMet y))

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
