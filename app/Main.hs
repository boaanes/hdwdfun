{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications  #-}
module Main
    ( ConstraintSystem (..)
    , main
    ) where
import           Algs                  (concatExprsInMethodList, getLabels,
                                        methodsToEnforce, plan)
import           Control.Applicative   ((<|>))
import           Control.Monad
import           Control.Monad.State
import           Data.Foldable ( traverse_, find )
import           Data.List             (intercalate)
import           Data.Map              (Map)
import qualified Data.Map              as Map
import           HotDrink              (Constraint (..), VertexType, eval,
                                        methodToGraph)
import           MethodParser          (Expr (..), Value (..), parseExpr)
import           PrettyPrinter         (prettyPrintConstraint)
import           System.IO
import           Text.Megaparsec       (parse)
import           Text.Megaparsec.Error (errorBundlePretty)
import           Text.Read             (readMaybe)

data Component
  = Component
      { identifier  :: Int
      , variables   :: Map String (Maybe Value)
      , constraints :: [Constraint]
      , strength    :: [String]
      }
    deriving (Show, Eq)

data ConstraintSystem where
  ConstraintSystem :: {components :: [Component]} -> ConstraintSystem deriving (Show)

readValue :: String -> Maybe Value
readValue s = (DoubleVal <$> readMaybe s) <|> (BoolVal <$> readMaybe s)

processInput :: String -> StateT ConstraintSystem IO ()
processInput input = do
    case words input of
        ["comp"] -> do
            comps <- gets components
            if null comps
               then addEmptyComponent
               else do
                   let lastComp = last comps
                   modify $ \cs -> cs { components = components cs ++ [lastComp { identifier = identifier lastComp + 1 }] }
            liftIO $ putStrLn "Added component"
        ["var", var, val] -> do
            case readValue val of
                Nothing -> liftIO $ putStrLn "Couldnt parse the value"
                Just v -> do
                    comps <- gets components
                    traverse_ (\c -> addVariableToComponent c var (Just v)) comps
                    liftIO $ putStrLn $ "Added variable: " ++ var ++ " = " ++ val
        ["constr", nMethodsStr] -> do
            case readMaybe @Integer nMethodsStr of
                Just n -> do
                    comps <- gets components
                    traverse_ addEmptyConstraintToComponent  comps
                    traverse_ (const inputMethod) [1..n]
                    liftIO $ putStrLn $ "Added constraint with " ++ nMethodsStr ++ " methods"
                _ -> liftIO $ putStrLn "Couldnt parse the id or the number of methods"
        ["update", ident, var, val] -> do
            case (readMaybe ident, readValue val) of
                (Just n, Just v) -> do
                    comps <- gets components
                    let comp = find (\c -> identifier c == n) comps
                    case comp of
                        Nothing -> liftIO $ putStrLn "Couldnt find component"
                        Just c -> do
                            addVariableToComponent c var (Just v)
                            modifyStrengthOfComponent c var
                            liftIO $ putStrLn $ "Updated variable: " ++ var ++ " = " ++ val
                _ -> liftIO $ putStrLn "Couldnt parse id or the value"
        ["delete", ident, var] -> do
            case readMaybe ident of
                (Just n) -> do
                    deleteVariableFromComponent n var
                    liftIO $ putStrLn $ "Deleted variable: " ++ var
                _ -> liftIO $ putStrLn "Couldnt parse id"
        ["delete", ident] -> do
            case readMaybe ident of
                (Just n) -> do
                    deleteComponent n
                    liftIO $ putStrLn "Deleted component"
                _ -> liftIO $ putStrLn "Couldnt parse id"
        ["show", "comp"] -> showComponents
        ["show", "var", ident] -> do
            case readMaybe ident of
                (Just n) -> do
                    showVariablesOfComponent n
                _ -> liftIO $ putStrLn "Couldnt parse id"
        ["show", "constr", ident] -> do
            case readMaybe ident of
                (Just n) -> do
                    showConstraintsOfComponent n
                _ -> liftIO $ putStrLn "Couldnt parse id"
        ["show", "strength", ident] -> do
            comps <- gets components
            let comp = find (\c -> identifier c == read ident) comps
            case comp of
                (Just c) -> do
                    showStrengthOfComponent c
                _ -> liftIO $ putStrLn "Couldnt find component"
        ["show", "plan", ident] -> do
            case readMaybe ident of
                (Just n) -> do
                    showPlanOfComponent n
                _ -> liftIO $ putStrLn "Couldnt parse id"
        ["run", ident] ->
            case readMaybe ident of
                (Just n) -> do
                    enforcePlan n
                _ -> liftIO $ putStrLn "Couldnt parse id"
        ["help"] -> do
            liftIO $ putStrLn "Commands:"
            liftIO $ putStrLn "var <name> <value> - add a variable"
            liftIO $ putStrLn "constr <n> - add a constraint with n methods"
            liftIO $ putStrLn "update <name> <value> - update a variable"
            liftIO $ putStrLn "delete <name> - delete a variable"
            liftIO $ putStrLn "show var - show all variables"
            liftIO $ putStrLn "show var <name> - show variable with a given name"
            liftIO $ putStrLn "show constr - show all constraints"
            liftIO $ putStrLn "show strength - show the current strength of variables in descending order"
            liftIO $ putStrLn "show plan - show the plan to be computed based on the current strength"
            liftIO $ putStrLn "run - enforce the current plan"
            liftIO $ putStrLn "help - show this message"
            liftIO $ putStrLn "exit - exit the program"
        ["exit"] -> return ()
        _ -> liftIO $ putStrLn "Unknown command"

addEmptyComponent :: StateT ConstraintSystem IO ()
addEmptyComponent = do
    modify $ \s -> s { components = components s ++ [Component (length (components s)) Map.empty [] []] }

deleteComponent :: Int -> StateT ConstraintSystem IO ()
deleteComponent i = do
    modify $ \s -> s { components = filter (\c -> identifier c /= i) (components s) }

showComponents :: StateT ConstraintSystem IO ()
showComponents = do
    cs <- gets components
    liftIO $ putStrLn $ "Components: " ++ intercalate ", " (map (show . identifier) cs)

addVariableToComponent :: Component -> String -> Maybe Value -> StateT ConstraintSystem IO ()
addVariableToComponent component name val = do
    modify $ \s -> s { components = map (\c -> if c == component then c { variables = Map.insert name val (variables c), strength = name : filter (/= name) (strength c) } else c) (components s) }

deleteVariableFromComponent :: Int -> String -> StateT ConstraintSystem IO ()
deleteVariableFromComponent i name = do
    modify $ \s -> s { components = map (\c -> if identifier c == i then c { variables = Map.delete name (variables c) } else c) (components s) }

modifyStrengthOfComponent :: Component -> String -> StateT ConstraintSystem IO ()
modifyStrengthOfComponent component name = do
    modify $ \s -> s { components = map (\c -> if c == component then c { strength = name : filter (/= name) (strength c) } else c) (components s) }

addEmptyConstraintToComponent :: Component -> StateT ConstraintSystem IO ()
addEmptyConstraintToComponent component = do
    modify $ \s -> s { components = map (\c -> if c == component then c { constraints = Constraint [] : constraints c } else c) (components s) }

showVariablesOfComponent :: Int -> StateT ConstraintSystem IO ()
showVariablesOfComponent i = do
    cs <- gets components
    let comp = find (\c -> identifier c == i) cs
    maybe (liftIO $ putStrLn $ "Component with id " ++ show i ++ " not found") (liftIO . mapM_ (\(name, val) -> putStrLn $ name ++ " = " ++ show val) . Map.toList . variables) comp

showConstraintsOfComponent :: Int -> StateT ConstraintSystem IO ()
showConstraintsOfComponent i = do
    cs <- gets components
    let comp = find (\c -> identifier c == i) cs
    maybe (liftIO $ putStrLn $ "Component with id " ++ show i ++ " not found") (liftIO . mapM_ (\c -> putStrLn $ prettyPrintConstraint c ++ "\n" ++ replicate 80 '-') . constraints) comp

showStrengthOfComponent :: Component -> StateT ConstraintSystem IO ()
showStrengthOfComponent comp = do
    liftIO $ putStrLn $ "Strength: " ++ show (strength comp)

showPlanOfComponent :: Int -> StateT ConstraintSystem IO ()
showPlanOfComponent i = do
    cs <- gets components
    let comp = find (\c -> identifier c == i) cs
    case comp of
        Nothing -> liftIO $ putStrLn $ "Component with id " ++ show i ++ " not found"
        Just c -> do
            liftIO $ putStrLn $ "Strength: " ++ show (strength c)
            liftIO $ putStrLn $ "Constraints: " ++ show (constraints c)
            maybe (liftIO $ putStrLn $ "No plan found" ++ show i ++ " not found") (liftIO . putStrLn . ("Plan: " ++) . intercalate " -> " . getLabels . Just) (computePlan (strength c) (constraints c))

computePlan :: [String] -> [Constraint] -> Maybe [VertexType]
computePlan stay cs = methodsToEnforce $ plan order $ mconcat cs
  where
    order = map (\s -> Constraint [methodToGraph [] ("m" ++ s, [(s, Var s)])]) stay

enforcePlan :: Int -> StateT ConstraintSystem IO ()
enforcePlan i = do
    comps <- gets components
    let comp = find (\c -> identifier c == i) comps
    case comp of
        Nothing -> liftIO $ putStrLn $ "Component with id " ++ show i ++ " not found"
        Just c -> do
            let st = strength c
            let cs = constraints c
            maybe
                (liftIO $ putStrLn "No plan found")
                (\m -> do
                    liftIO $ print m
                    liftIO $ putStrLn "am at here"
                    enforce i (concatExprsInMethodList m)
                    liftIO $ putStrLn $ "Enforced plan: " ++ intercalate " -> " (getLabels $ Just m)
                ) (computePlan st cs)

enforce :: Int -> [(String, Expr)] -> StateT ConstraintSystem IO ()
enforce i = traverse_ (\(name, e) -> do
    comps <- gets components
    let comp = find (\c -> identifier c == i) comps
    case comp of
        Nothing -> liftIO $ putStrLn $ "Component with id " ++ show i ++ " not found"
        Just c -> do
            let vars = variables c
                newVal = eval e vars
            modify $ \s -> s { components = map (\c' -> if identifier c' == i then c' { variables = Map.insert name newVal (variables c') } else c') (components s) }
    )

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

inputMethod :: StateT ConstraintSystem IO ()
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
    modify $ \s -> s { components = map (\c -> c { constraints = Constraint (methodGraph : unConstraint (head $ constraints c)) : drop 1 (constraints c) }) (components s) }

prompt :: IO String
prompt = do
    putStr "\ESC[32m$ "
    hFlush stdout
    input <- getLine
    putStr "\ESC[0m"
    return input

userInputLoop :: StateT ConstraintSystem IO ()
userInputLoop = do
    input <- liftIO prompt
    processInput input
    unless (input == "exit") userInputLoop


testVars :: Map String (Maybe Value)
testVars = Map.fromList [("w", Just (DoubleVal 10)), ("h", Just (DoubleVal 10)), ("a", Just (DoubleVal 100)), ("p", Just (DoubleVal 40))]

testCons :: [Constraint]
testCons =
    [ Constraint
        [ methodToGraph ["w", "h"] ("m1", [("a", BinOp "*" (Var "w") (Var "h"))])
        , methodToGraph ["a"] ("m2", [("w", UnOp "sqrt" (Var "a")), ("h", UnOp "sqrt" (Var "a"))])
        ]
    , Constraint
        [ methodToGraph ["w", "h"] ("m3", [("p", BinOp "*" (Lit (DoubleVal 2)) (BinOp "+" (Var "w")  (Var "h")))])
        , methodToGraph ["w", "p"] ("m4", [("h", BinOp "-" (BinOp "/" (Var "p") (Lit (DoubleVal 2))) (Var "w"))])
        , methodToGraph ["h", "p"] ("m5", [("w", BinOp "-" (BinOp "/" (Var "p") (Lit (DoubleVal 2))) (Var "h"))])
        ]
    ]

testOrder :: [String]
testOrder = ["a", "p", "w", "h"]

main :: IO ()
main = do
    putStrLn "Welcome to HotDrink"
    putStrLn "Type 'help' for a list of commands"
    evalStateT userInputLoop (ConstraintSystem [])
    putStrLn "Goodbye"
