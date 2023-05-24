{-# LANGUAGE GADTs            #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
module CLI
    ( Mode (..)
    , userInputLoop
    ) where
import           AST                   (Expr (..), Value (..), parseExpr)
import           Algs                  (computePlan, concatExprsInMethodList,
                                        getLabels, methodsToEnforce, plan)
import           Control.Applicative   ((<|>))
import           Control.Monad
import           Control.Monad.State
import           Data.Foldable         (find, foldl', traverse_)
import           Data.Functor          ((<&>))
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

data Mode = Normal | Manual deriving (Eq, Show)

--- Pure helpers ---

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

insertAfter :: Component -> Component -> [Component] -> [Component]
insertAfter _ _ [] = []
insertAfter c1 c2 (c:cs)
  | c == c1 = c : c2 : cs
  | otherwise = c : insertAfter c1 c2 cs

getNext :: Eq a => a -> [a] -> Maybe a
getNext _ [] = Nothing
getNext _ [_] = Nothing
getNext e (x:y:xs)
  | e == x = Just y
  | otherwise = getNext e (y:xs)

getPrev :: Eq a => a -> [a] -> Maybe a
getPrev _ [] = Nothing
getPrev _ [_] = Nothing
getPrev e (x:y:xs)
  | e == y = Just x
  | otherwise = getPrev e (y:xs)

swapComponents :: Component -> Component -> [Component] -> [Component]
swapComponents _ _ [] = []
swapComponents c1 c2 (c:cs)
  | c == c1 = c2 : swapComponents c1 c2 cs
  | c == c2 = c1 : swapComponents c1 c2 cs
  | otherwise = c : swapComponents c1 c2 cs

getComponentWithBiggestId :: [Component] -> Component
getComponentWithBiggestId = foldl' (\c1 c2 -> if identifier c1 > identifier c2 then c1 else c2) (Component 0 Map.empty [] [])

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
    maybe (putLnIO "No plan found") (enforceMethods c . concatExprsInMethodList) (computePlan st cs)

enforceMethods :: Component -> [(String, Expr)] -> StateT ConstraintSystem IO ()
enforceMethods c = traverse_ (\(name, e) -> modify $ \s -> s { components = map (\c' -> if identifier c' == identifier c then c' { variables = Map.insert name (eval e (variables c')) (variables c') } else c') (components s) })

enforceIntercalatingConstraint :: Int -> StateT ConstraintSystem IO ()
enforceIntercalatingConstraint i = do
    comps <- gets components
    inter <- gets intercalatingConstraints
    let comp = find (\c -> identifier c == i) comps
    case comp of
        Nothing -> putLnIO $ "Component with id " ++ show i ++ " not found"
        Just c -> do
            let fromVars = variables c
                mte = concatExprsInMethodList $ fromMaybe [] $ methodsToEnforce $ plan [] $ mconcat inter
                newVals = map (\(name, e) -> (name, eval e fromVars)) mte
                nextElem = getNext c comps
            case nextElem of
                Nothing -> return () -- last element
                Just ne -> do
                    modify $ \s -> s { components = map (\c' -> if identifier c' == identifier ne then c' { variables = Map.union (Map.fromList newVals) (variables c') } else c') (components s) }

satisfyInter :: String -> StateT ConstraintSystem IO ()
satisfyInter ident = do
    case readMaybe @Int ident of
        (Just n) -> do
            comps <- gets components
            traverse_ (\c -> satisfy c >> enforceIntercalatingConstraint (identifier c)) $ dropWhile (\c -> identifier c /= n) comps
        _        -> putLnIO "Couldnt parse id"

inputExpr :: String -> IO (String, Expr)
inputExpr name = do
    putStrLn $ "Enter expression for " ++ name ++ ":"
    input <- prompt
    case parse parseExpr "" input of
        Right e -> do
            putLnIO "Parse success"
            return (name, e)
        Left bundle -> do
            putStr (errorBundlePretty bundle)
            inputExpr name

inputMethod :: IO MethodGraph
inputMethod = do
    putLnIO "Enter name of method:"
    name <- liftIO prompt
    putLnIO "Enter space separated input names to method:"
    inputsStr <- liftIO prompt
    putLnIO "Enter output variables to method:"
    outputsStr <- liftIO prompt
    let inputs = words inputsStr
        outputs = words outputsStr
    exprs <- liftIO $ traverse inputExpr outputs
    let method = (name, exprs)
        methodGraph = methodToGraph inputs method
    return methodGraph


--- Process input from user ---

processInput :: Mode -> String -> StateT ConstraintSystem IO Mode
processInput mode input = do
    case words input of
        ["manual"] -> putLnIO "Entering manual mode" >> return Manual
        ["normal"] -> do
            comps <- gets components
            satisfyInter ((show . identifier . head) comps) >> putLnIO "Entering normal mode" >> return Normal
        ["addComp"] -> do
            comps <- gets components
            if null comps
                then modify $ \s -> s { components = components s ++ [Component (length (components s)) Map.empty [] []] }
                else do
                    let lastComp = last comps
                    modify $ \cs -> cs { components = components cs ++ [lastComp { identifier = identifier lastComp + 1 }] }
                    satisfyInter $ (show . identifier . head) comps
            putLnIO "Added component"
            return mode
        ["list", nCompsStr] -> do
            comps <- gets components
            if null comps
                then case readMaybe @Int nCompsStr of
                    Just n -> do
                        let newComps = fmap (\i -> Component i Map.empty [] []) [0..n-1]
                        modify $ \cs -> cs { components = newComps }
                        putLnIO $ "Added " ++ nCompsStr ++ " components"
                    _ -> putLnIO "Couldnt parse the number of components"
                else putLnIO "There are already components defined"
            return mode
        ["addVar", var, val] -> do
            case readValue val of
                Nothing -> putLnIO "Couldnt parse the value"
                Just v -> do
                    comps <- gets components
                    let newComps = addVariableToComponent var (Just v) <$> comps
                    modify $ \cs -> cs { components = newComps }
                    putLnIO $ "Added variable: " ++ var ++ " = " ++ val
            return mode
        ["addConstr", nMethodsStr] -> do
            case readMaybe @Int nMethodsStr of
                Just n -> do
                    methodGraphs <- liftIO $ traverse (const inputMethod) [1..n]
                    modify $ \cs -> cs { components = fmap (\c -> c { constraints = Constraint methodGraphs : constraints c }) (components cs) }
                    comps <- gets components
                    satisfyInter $ (show . identifier . head) comps
                    putLnIO $ "Added constraint with " ++ nMethodsStr ++ " methods"
                _ -> putLnIO "Couldnt parse the id or the number of methods"
            return mode
        ["addInter", nMethodsStr] -> do
            case readMaybe @Int nMethodsStr of
                Just n -> do
                    methodGraphs <- liftIO $ traverse (const inputMethod) [1..n]
                    modify $ \cs -> cs { intercalatingConstraints = Constraint methodGraphs : intercalatingConstraints cs }
                    comps <- gets components
                    satisfyInter $ (show . identifier . head) comps
                    putLnIO $ "Added intercalating constraint with " ++ nMethodsStr ++ " methods"
                _ -> putLnIO "Couldnt parse the id or the number of methods"
            return mode
        ["updateVar", ident, var, val] -> do
            case readValue val of
                Just v -> do
                    maybeComp <- findComponent ident
                    case maybeComp of
                        Nothing -> putLnIO "Couldnt find component"
                        Just c -> do
                            let newComp = addVariableToComponent var (Just v) c
                            modify $ \cs -> cs { components = fmap (\c' -> if identifier c' == identifier c then newComp else c') (components cs) }
                            unless (mode == Manual) $ satisfyInter ident
                            putLnIO $ "Updated variable: " ++ var ++ " = " ++ val
                _ -> putLnIO "Couldnt parse id or the value"
            return mode
        ["deleteVar", var] -> do
            comps <- gets components
            let newComps = deleteVariableFromComponent var <$> comps
            modify $ \cs -> cs { components = newComps }
            putLnIO $ "Deleted variable: '" ++ var ++ "' from all components"
            return mode
        ["insertAfter", ident] -> do
            maybePrecedingComp <- findComponent ident
            case maybePrecedingComp of
                Nothing -> putLnIO "Couldnt find component"
                Just preceedingComp -> do
                    comps <- gets components
                    let newComp = preceedingComp { identifier = (identifier . getComponentWithBiggestId) comps + 1 }
                        newComps = insertAfter preceedingComp newComp comps
                    modify $ \cs -> cs { components = newComps }
                    unless (mode == Manual) $ satisfyInter ident
                    putLnIO $ "Inserted component after component with id " ++ show (identifier preceedingComp)
            return mode
        ["swap", identA, identB] -> do
            maybeCompA <- findComponent identA
            maybeCompB <- findComponent identB
            case (maybeCompA, maybeCompB) of
                (Just compA, Just compB) -> do
                    comps <- gets components
                    let newComps = swapComponents compA compB comps
                    modify $ \cs -> cs { components = newComps }
                    putLnIO $ "Swapped components with ids " ++ show (identifier compA) ++ " and " ++ show (identifier compB)
                    unless (mode == Manual) $ maybe (return ()) satisfyInter ((getPrev compA comps <&> (show . identifier)) <|> Just (show $ identifier compB))
                _ -> putLnIO "Couldnt find one or both components"
            return mode
        ["remove", ident] -> do
            maybeComp <- findComponent ident
            case maybeComp of
                Nothing -> putLnIO "Couldnt find component"
                Just c -> do
                    comps <- gets components
                    let newComps = filter ((/= identifier c) . identifier) comps
                    modify $ \cs -> cs { components = newComps }
                    putLnIO $ "Deleted component with id " ++ show (identifier c)
                    unless (mode == Manual) $ maybe (return ()) satisfyInter (getPrev c comps <&> (show . identifier))
                    when (null newComps) $ modify $ \cs -> cs { intercalatingConstraints = [] }
            return mode
        ["show", "comp"] -> do
            comps <- gets components
            putLnIO $ intercalate "\n\n" $ fmap showComponent comps
            return mode
        ["show", "var", ident] -> do
            maybeComp <- findComponent ident
            case maybeComp of
                Nothing -> putLnIO "Couldnt find component"
                Just c  -> putLnIO $ showVariablesOfComponent c
            return mode
        ["show", "constr"] -> do
            comps <- gets components
            let comp = safeHead comps
            case comp of
                Just c -> putLnIO $ showConstraintsOfComponent c
                _      -> putLnIO "No components defined"
            return mode
        ["show", "inter"] -> do
            cs <- gets intercalatingConstraints
            putLnIO $ intercalate "\n" $ fmap ((<> "\n" <> replicate 80 '-'). prettyPrintConstraint) cs
            return mode
        ["show", "strength", ident] -> do
            maybeComp <- findComponent ident
            case maybeComp of
                (Just c) -> putLnIO $ showStrengthOfComponent c
                _        -> putLnIO "Couldnt find component"
            return mode
        ["show", "plan", ident] -> do
            maybeComp <- findComponent ident
            case maybeComp of
                (Just c) -> putLnIO $ showPlanOfComponent c
                _        -> putLnIO "Couldnt find component"
            return mode
        ["runLocal", ident] -> do
            maybeComp <- findComponent ident
            case maybeComp of
                (Just c) -> satisfy c
                _        -> putLnIO "Couldnt find component"
            return mode
        ["runInter", "inter", ident] -> satisfyInter ident >> return mode
        ["satisty"] -> gets components >>= \comps -> satisfyInter ((show . identifier . head) comps) >> return mode
        ["help"] -> do
            putLnIO $ "\ESC[1;31mYou are in " <> show mode <> " mode\ESC[0m"
            putLnIO "\ESC[31mAvailable commands are:\ESC[0m"
            putLnIO "manual - enter manual mode, operations will not automatically satisfy the constraint system"
            putLnIO "normal - enter normal mode, operations will automatically satisfy the constraint system"
            putLnIO "addComp - add a component"
            putLnIO "list <n> - add n components"
            putLnIO "addVar <var> <val> - add a variable to all components"
            putLnIO "addConstr <n> - add a constraint with n methods to all components"
            putLnIO "addInter <n> - add an intercalating constraint with n methods"
            putLnIO "updateVar <id> <var> <val> - update a variable of a component"
            putLnIO "deleteVar <var> - delete a variable from all components"
            putLnIO "insertAfter <id> - insert a component after the component with the given id"
            putLnIO "swap <id> <id> - swap the positions two components"
            putLnIO "remove <id> - delete a component"
            putLnIO "show comp - show all components"
            putLnIO "show var <id> - show all variables of a component (all components have the same set of variables)"
            putLnIO "show constr - show the constraints of each component"
            putLnIO "show inter - show all intercalating constraints"
            putLnIO "show strength <id> - show the strength of the variables of a component"
            putLnIO "show plan <id> - show the current plan of a component"
            putLnIO "runLocal <id> - enforce the plan of a component"
            putLnIO "runInter <id> - satisfy the whole constraint system from the given component to the end"
            putLnIO "satisfy - satisfy the whole constraint system from the first component to the end"
            putLnIO "help - show this message"
            putLnIO "exit - exit the program"
            return mode
        ["exit"] -> return mode
        _ -> putLnIO "Unknown command" >> return mode

putLnIO :: MonadIO m => String -> m ()
putLnIO = liftIO . putStrLn

-- Prompt function
prompt :: IO String
prompt = do
    putStr "\ESC[32m$ "
    hFlush stdout
    input <- getLine
    putStr "\ESC[0m"
    return input


-- Main CLI loop
userInputLoop :: Mode -> StateT ConstraintSystem IO ()
userInputLoop mode = do
    input <- liftIO prompt
    newMode <- processInput mode input
    unless (input == "exit") $ userInputLoop newMode
