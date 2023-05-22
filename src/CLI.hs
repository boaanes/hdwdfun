{-# LANGUAGE GADTs            #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeApplications #-}
module CLI
    ( userInputLoop
    ) where
import           AST                   (Expr (..), Value (..), parseExpr)
import           Algs                  (computePlan, concatExprsInMethodList,
                                        getLabels, methodsToEnforce, plan)
import           Control.Applicative   ((<|>))
import           Control.Monad
import           Control.Monad.State
import           Data.Foldable         (find, foldl', traverse_)
import           Data.List             (intercalate)
import qualified Data.Map              as Map
import           Data.Maybe            (fromMaybe)
import           HotDrinkF             (Constraint (..), MethodGraph, eval,
                                        methodToGraph)
import           PrettyPrinter
import           System.IO
import           Text.Megaparsec       (parse)
import           Text.Megaparsec.Error (errorBundlePretty)
import           Text.Read             (readMaybe)
import           WarmDrinkF            (Component (..), ConstraintSystem (..))

--- Pure helpers ---

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

insertAt :: (Eq a) => Int -> a -> [a] -> [a]
insertAt _ x []     = [x]
insertAt 0 x xs     = x:xs
insertAt n x (y:ys) = y : insertAt (n-1) x ys

swapAt :: Int -> Int -> [a] -> [a]
swapAt i j xs = [get' k x | (k, x) <- zip [0..] xs]
    where get' k x | k == i = xs !! j
                  | k == j = xs !! i
                  | otherwise = x

getNext :: Eq a => a -> [a] -> Maybe a
getNext _ [] = Nothing
getNext _ [_] = Nothing
getNext e (x:y:xs)
  | e == x = Just y
  | otherwise = getNext e (y:xs)

addVariableToComponent :: String -> Maybe Value -> Component -> Component
addVariableToComponent name val component =
  component { variables = Map.insert name val (variables component)
            , strength = name : filter (/= name) (strength component)
            }

readValue :: String -> Maybe Value
readValue s = (DoubleVal <$> readMaybe s) <|> (BoolVal <$> readMaybe s)

deleteVariableFromComponent :: String -> Component -> Component
deleteVariableFromComponent name component =
  component { variables = Map.delete name (variables component)
            , strength = filter (/= name) (strength component)
            }

applyIntercalatingConstraint :: Constraint -> (Component, Component) -> Component
applyIntercalatingConstraint cs (c1, c2) =
    let vars1 = variables c1
        mte = concatExprsInMethodList $ fromMaybe [] $ methodsToEnforce $ plan [] cs
        newVals = map (\(name, e) -> (name, eval e vars1)) mte
    in c2 { variables = Map.union (Map.fromList newVals) (variables c2) }

applyAllInterclatingConstraints :: [Constraint] -> (Component, Component) -> Component
applyAllInterclatingConstraints inters comps =
    foldl' (\c cs -> applyIntercalatingConstraint cs (c, snd comps)) (fst comps) inters

--- IO helpers ---

findComponent :: String -> StateT ConstraintSystem IO (Maybe Component)
findComponent ident = do
    comps <- gets components
    return $ case readMaybe ident of
        Just identInt -> find (\c -> identifier c == identInt) comps
        Nothing       -> Nothing

satisfy :: Component -> StateT ConstraintSystem IO ()
satisfy c = do
    let st = strength c
    let cs = constraints c
    maybe
        (liftIO $ putStrLn "No plan found")
        (\m -> do
            enforceMethods c (concatExprsInMethodList m)
            liftIO $ putStrLn $ "Enforced plan: " ++ intercalate " -> " (getLabels $ Just m) ++ " on component " ++ show (identifier c)
        ) (computePlan st cs)

enforceMethods :: Component -> [(String, Expr)] -> StateT ConstraintSystem IO ()
enforceMethods c = traverse_ (\(name, e) -> modify $ \s -> s { components = map (\c' -> if identifier c' == identifier c then c' { variables = Map.insert name (eval e (variables c')) (variables c') } else c') (components s) })

enforceIntercalatingConstraint :: Int -> StateT ConstraintSystem IO ()
enforceIntercalatingConstraint i = do
    comps <- gets components
    inter <- gets intercalatingConstraints
    let comp = find (\c -> identifier c == i) comps
    case comp of
        Nothing -> liftIO $ putStrLn $ "Component with id " ++ show i ++ " not found"
        Just c -> do
            let fromVars = variables c
                mte = concatExprsInMethodList $ fromMaybe [] $ methodsToEnforce $ plan [] $ mconcat inter
                newVals = map (\(name, e) -> (name, eval e fromVars)) mte
                nextElem = getNext c comps
            case nextElem of
                Nothing -> return () -- last element
                Just ne -> do
                    modify $ \s -> s { components = map (\c' -> if identifier c' == identifier ne then c' { variables = Map.union (Map.fromList newVals) (variables c') } else c') (components s) }
                    liftIO $ putStrLn $ "Enforcing intercalating constraints on component " ++ show i ++ " to component " ++ show (i + 1)

inputExpr :: String -> IO (String, Expr)
inputExpr name = do
    putStrLn $ "Enter expression for " ++ name ++ ":"
    input <- prompt
    case parse parseExpr "" input of
        Right e -> do
            liftIO $ putStrLn "Parse success"
            return (name, e)
        Left bundle -> do
            putStr (errorBundlePretty bundle)
            inputExpr name

inputMethod :: IO MethodGraph
inputMethod = do
    liftIO $ putStrLn "Enter name of method:"
    name <- liftIO prompt
    liftIO $ putStrLn "Enter space separated input names to method:"
    inputsStr <- liftIO prompt
    liftIO $ putStrLn "Enter output variables to method:"
    outputsStr <- liftIO prompt
    let inputs = words inputsStr
        outputs = words outputsStr
    exprs <- liftIO $ traverse inputExpr outputs
    let method = (name, exprs)
        methodGraph = methodToGraph inputs method
    return methodGraph


--- Process input from user ---

processInput :: String -> StateT ConstraintSystem IO ()
processInput input = do
    case words input of
        ["comp"] -> do
            comps <- gets components
            if null comps
               then modify $ \s -> s { components = components s ++ [Component (length (components s)) Map.empty [] []] }
               else do
                   let lastComp = last comps
                   modify $ \cs -> cs { components = components cs ++ [lastComp { identifier = identifier lastComp + 1 }] }
            liftIO $ putStrLn "Added component"
        ["list", nCompsStr] -> do
            comps <- gets components
            if null comps
                then case readMaybe @Int nCompsStr of
                    Just n -> do
                        let newComps = fmap (\i -> Component i Map.empty [] []) [0..n-1]
                        modify $ \cs -> cs { components = newComps }
                        liftIO $ putStrLn $ "Added " ++ nCompsStr ++ " components"
                    _ -> liftIO $ putStrLn "Couldnt parse the number of components"
                else liftIO $ putStrLn "There are already components defined"
        ["var", var, val] -> do
            case readValue val of
                Nothing -> liftIO $ putStrLn "Couldnt parse the value"
                Just v -> do
                    comps <- gets components
                    let newComps = addVariableToComponent var (Just v) <$> comps
                    modify $ \cs -> cs { components = newComps }
                    liftIO $ putStrLn $ "Added variable: " ++ var ++ " = " ++ val
        ["constr", nMethodsStr] -> do
            case readMaybe @Int nMethodsStr of
                Just n -> do
                    methodGraphs <- liftIO $ traverse (const inputMethod) [1..n]
                    modify $ \cs -> cs { components = fmap (\c -> c { constraints = Constraint methodGraphs : constraints c }) (components cs) }
                    liftIO $ putStrLn $ "Added constraint with " ++ nMethodsStr ++ " methods"
                _ -> liftIO $ putStrLn "Couldnt parse the id or the number of methods"
        ["inter", nMethodsStr] -> do
            case readMaybe @Int nMethodsStr of
                Just n -> do
                    methodGraphs <- liftIO $ traverse (const inputMethod) [1..n]
                    modify $ \cs -> cs { intercalatingConstraints = Constraint methodGraphs : intercalatingConstraints cs }
                    liftIO $ putStrLn $ "Added intercalating constraint with " ++ nMethodsStr ++ " methods"
                _ -> liftIO $ putStrLn "Couldnt parse the id or the number of methods"
        ["update", ident, var, val] -> do
            case readValue val of
                Just v -> do
                    maybeComp <- findComponent ident
                    case maybeComp of
                        Nothing -> liftIO $ putStrLn "Couldnt find component"
                        Just c -> do
                            let newComp = addVariableToComponent var (Just v) c
                            modify $ \cs -> cs { components = fmap (\c' -> if identifier c' == identifier c then newComp else c') (components cs) }
                            liftIO $ putStrLn $ "Updated variable: " ++ var ++ " = " ++ val
                _ -> liftIO $ putStrLn "Couldnt parse id or the value"
        ["insert", ident] -> do
            case readMaybe ident of
                (Just n) -> do
                    comps <- gets components
                    if n >= length comps
                        then liftIO $ putStrLn "Index out of bounds"
                        else do
                            maybeComp <- findComponent ident
                            case maybeComp of
                                (Just c) -> do
                                    let newComp = c { identifier = length comps }
                                        newComps = insertAt n newComp comps
                                    modify $ \cs -> cs { components = newComps }
                                    liftIO $ putStrLn $ "Inserted component at index " ++ show n
                                Nothing -> liftIO $ putStrLn "Couldnt find component"
                Nothing -> liftIO $ putStrLn "Couldnt parse id"
        ["swap", ident] -> do
            case readMaybe @Int ident of
                (Just n) -> do
                    comps <- gets components
                    if n >= length comps - 1
                        then liftIO $ putStrLn "Index out of bounds"
                        else do
                            let newComps = swapAt n (n + 1) comps
                            modify $ \cs -> cs { components = newComps }
                            liftIO $ putStrLn $ "Swapped component at index " ++ show n ++ " with component at index " ++ show (n + 1)
                Nothing -> liftIO $ putStrLn "Couldnt parse id"
        ["delete", "var", var] -> do
            comps <- gets components
            let newComps = deleteVariableFromComponent var <$> comps
            modify $ \cs -> cs { components = newComps }
            liftIO $ putStrLn $ "Deleted variable: '" ++ var ++ "' from all components"
        ["delete", "comp", ident] -> do
            case readMaybe ident of
                (Just n) -> do
                    modify $ \s -> s { components = filter (\c -> identifier c /= n) (components s) }
                    liftIO $ putStrLn $ "Deleted component with id: " ++ ident
                _ -> liftIO $ putStrLn "Couldnt parse id"
        ["show", "comp"] -> do
            comps <- gets components
            liftIO $ putStrLn $ intercalate "\n\n" $ fmap showComponent comps
        ["show", "var", ident] -> do
            maybeComp <- findComponent ident
            case maybeComp of
                Nothing -> liftIO $ putStrLn "Couldnt find component"
                Just c  -> liftIO $ putStrLn $ showVariablesOfComponent c
        ["show", "constr"] -> do
            comps <- gets components
            let comp = safeHead comps
            case comp of
                Just c -> liftIO $ putStrLn $ showConstraintsOfComponent c
                _      -> liftIO $ putStrLn "No components defined"
        ["show", "inter"] -> do
            cs <- gets intercalatingConstraints
            liftIO $ putStrLn $ intercalate "\n" $ fmap ((<> "\n" <> replicate 80 '-'). prettyPrintConstraint) cs
        ["show", "strength", ident] -> do
            maybeComp <- findComponent ident
            case maybeComp of
                (Just c) -> liftIO $ putStrLn $ showStrengthOfComponent c
                _        -> liftIO $ putStrLn "Couldnt find component"
        ["show", "plan", ident] -> do
            maybeComp <- findComponent ident
            case maybeComp of
                (Just c) -> liftIO $ putStrLn $ showPlanOfComponent c
                _        -> liftIO $ putStrLn "Couldnt find component"
        ["run", ident] -> do
            maybeComp <- findComponent ident
            case maybeComp of
                (Just c) -> satisfy c
                _        -> liftIO $ putStrLn "Couldnt find component"
        ["run", "inter", ident] -> do
            case readMaybe @Int ident of
                (Just n) -> do
                    comps <- gets components
                    traverse_ (\c -> satisfy c >> enforceIntercalatingConstraint (identifier c)) $ drop n comps
                _ -> liftIO $ putStrLn "Couldnt parse id"
        ["help"] -> do
            liftIO $ putStrLn "Commands:"
            liftIO $ putStrLn "cowboy - enter manual mode, operations will not automatically satisfy the constraint system"
            liftIO $ putStrLn "comp - add a component"
            liftIO $ putStrLn "list <n> - add n components"
            liftIO $ putStrLn "var <var> <val> - add a variable to all components"
            liftIO $ putStrLn "constr <n> - add a constraint with n methods to all components"
            liftIO $ putStrLn "inter <n> - add an intercalating constraint with n methods"
            liftIO $ putStrLn "update <id> <var> <val> - update a variable of a component"
            liftIO $ putStrLn "insert <index> - insert a component at the given index"
            liftIO $ putStrLn "swap <index> - swap a component at the given index with the next one"
            liftIO $ putStrLn "delete var <var> - delete a variable from a component"
            liftIO $ putStrLn "delete comp <id> - delete a component"
            liftIO $ putStrLn "show comp - show all components"
            liftIO $ putStrLn "show var <id> - show all variables of a component (all components have the same set of variables)"
            liftIO $ putStrLn "show constr - show the constraints of each component"
            liftIO $ putStrLn "show inter - show all intercalating constraints"
            liftIO $ putStrLn "show strength <id> - show the strength of the variables of a component"
            liftIO $ putStrLn "show plan <id> - show the current plan of a component"
            liftIO $ putStrLn "run <id> - enforce the plan of a component"
            liftIO $ putStrLn "run inter <id> - satisfy the whole constraint system from the given component to the end"
            liftIO $ putStrLn "help - show this message"
            liftIO $ putStrLn "exit - exit the program"
        ["exit"] -> return ()
        _ -> liftIO $ putStrLn "Unknown command"

-- Prompt function
prompt :: IO String
prompt = do
    putStr "\ESC[32m$ "
    hFlush stdout
    input <- getLine
    putStr "\ESC[0m"
    return input


-- Main CLI loop
userInputLoop :: StateT ConstraintSystem IO ()
userInputLoop = do
    input <- liftIO prompt
    processInput input
    unless (input == "exit") userInputLoop
