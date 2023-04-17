module Main
    ( computePlan
    , main
    ) where
import           Algs                (concatExprsInMethodList, getLabels,
                                      methodsToEnforce, plan)
import           Control.Monad
import           Control.Monad.State
import           Data.Foldable       (traverse_)
import           Data.List           (intercalate)
import           Data.Map            (Map)
import qualified Data.Map            as Map
import           HotDrink            (Constraint (..), VertexType, eval,
                                      methodToGraph)
import           MethodParser        (Expr (..), Parser (runParser), expr)
import           System.IO

data ConstraintSystem
  = ConstraintSystem
      { variables   :: Map String (Maybe Double)
      , constraints :: [Constraint]
      , strength    :: [String]
      }

processInput :: String -> StateT ConstraintSystem IO ()
processInput input = do
    case words input of
        ["addVar", var, val] -> do
            modify $ \s -> s { variables = Map.insert var (Just $ read val) (variables s) }
            modify $ \s -> s { strength = var : strength s }
            liftIO $ putStrLn $ "Added variable: " ++ var ++ " = " ++ val
        ["addCons", nMethodsStr] -> do
            case (reads nMethodsStr :: [(Int, String)]) of
                [(n, "")] -> do
                    modify $ \s -> s { constraints = Constraint [] : constraints s }
                    traverse_ (const inputMethod) [1..n]
                    liftIO $ putStrLn $ "Added constraint with " ++ nMethodsStr ++ " methods"
                _ -> liftIO $ putStrLn "Couldnt parse the number of methods"
        ["update", var, val] -> do
            modify $ \s -> s { variables = Map.insert var (Just $ read val) (variables s) }
            modify $ \s -> s { strength = var : filter (/= var) (strength s) }
            liftIO $ putStrLn $ "Updated variable: " ++ var ++ " = " ++ val
        ["delete", var] -> do
            modify $ \s -> s { variables = Map.delete var (variables s) }
            liftIO $ putStrLn $ "Deleted variable: " ++ var
        -- pretty print variables
        ["readVars"] -> do
            vars <- gets variables
            liftIO $ mapM_ (\(name, val) -> putStrLn $ name ++ " = " ++ show val) (Map.toList vars)
        ["readCons"] -> do
            cons <- gets constraints
            liftIO $ putStrLn $ "Constraints: " ++ show cons
        ["readStay"] -> do
            stay <- gets strength
            liftIO $ putStrLn $ "Stay variables: " ++ show stay
        ["plan"] -> do
            cons <- gets constraints
            st <- gets strength
            case computePlan st cons of
                Just m  -> liftIO $ putStrLn $ "Plan: " ++ intercalate " -> " (getLabels $ Just m)
                Nothing -> liftIO $ putStrLn "No plan found"
        ["satisfy"] -> do
            enforcePlan
        ["updateAndSatisfy", var, val] -> do
            modify $ \s -> s { variables = Map.insert var (Just $ read val) (variables s) }
            modify $ \s -> s { strength = var : filter (/= var) (strength s) }
            enforcePlan
        ["help"] -> do
            liftIO $ putStrLn "Commands:"
            liftIO $ putStrLn "addVar <var> <val> - add a variable with a value"
            liftIO $ putStrLn "addCons <n> - add a constraint with n methods"
            liftIO $ putStrLn "update <var> <val> - update a variable with a value"
            liftIO $ putStrLn "delete <var> - delete a variable"
            liftIO $ putStrLn "readVars - read all variables"
            liftIO $ putStrLn "readCons - read all constraints"
            liftIO $ putStrLn "readStay - read all stay variables"
            liftIO $ putStrLn "plan - compute a plan"
            liftIO $ putStrLn "satisfy - enforce the plan"
            liftIO $ putStrLn "updateAndSatisfy <var> <val> - update a variable and enforce the plan"
            liftIO $ putStrLn "help - print this help"
            liftIO $ putStrLn "exit - exit the program"
        ["exit"] -> return ()
        _ -> liftIO $ putStrLn "Unknown command"

computePlan :: [String] -> [Constraint] -> Maybe [VertexType]
computePlan stay cons = methodsToEnforce $ plan order $ mconcat cons
  where
    order = map (\s -> Constraint [methodToGraph [] ("m" ++ s, [(s, Var s)])]) stay

enforcePlan :: StateT ConstraintSystem IO ()
enforcePlan = do
    cons <- gets constraints
    st <- gets strength
    case computePlan st cons of
        Just m  -> do
            let ms = concatExprsInMethodList m
            enforce ms
            liftIO $ putStrLn $ "Enforced plan: " ++ intercalate " -> " (getLabels $ Just m)
        Nothing -> liftIO $ putStrLn "No plan found"

enforce :: [(String, Expr)] -> StateT ConstraintSystem IO ()
enforce = traverse_ (\(name, e) -> do
    vars <- gets variables
    let newVal = eval vars e
    modify $ \s -> s { variables = Map.insert name (Just newVal) (variables s) }
    )

inputExpr :: String -> IO (String, Expr)
inputExpr name = do
    putStrLn $ "Enter expression for " ++ name ++ ":"
    input <- prompt
    case runParser (expr :: Parser Char String Expr) input of
        Right (e, "") -> return (name, e)
        Right (_, trail) -> do
            putStrLn $ "Parse error: " ++ trail
            inputExpr name
        Left err -> do
            putStrLn $ "Parse error: " ++ show err
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
    exprs <- liftIO $ traverse inputExpr outputs
    let method = (name, exprs)
        methodGraph = methodToGraph inputs method
    modify $ \s -> s { constraints = Constraint (methodGraph : unConstraint (head $ constraints s)) : drop 1 (constraints s) }

    liftIO $ putStrLn "Parse success"

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

testVars :: Map String (Maybe Double)
testVars = Map.fromList [("w", Just 10), ("h", Just 10), ("a", Just 100), ("p", Just 40)]

testCons :: [Constraint]
testCons =
    [ Constraint
        [ methodToGraph ["w", "h"] ("m1", [("a", BinOp "*" (Var "w") (Var "h"))])
        , methodToGraph ["a"] ("m3", [("w", Sqrt (Var "a")), ("h", Sqrt (Var "a"))])
        ]
    , Constraint
        [ methodToGraph ["w", "h"] ("m2", [("p", BinOp "*" (Lit 2) (BinOp "+" (Var "w")  (Var "h")))])
        , methodToGraph ["w", "p"] ("m4", [("h", BinOp "-" (BinOp "/" (Var "p") (Lit 2)) (Var "w"))])
        , methodToGraph ["h", "p"] ("m5", [("w", BinOp "-" (BinOp "/" (Var "p") (Lit 2)) (Var "h"))])
        ]
    ]

testOrder :: [String]
testOrder = ["a", "p", "w", "h"]

main :: IO ()
main = do
    putStrLn "Welcome to HotDrink"
    evalStateT userInputLoop (ConstraintSystem testVars testCons testOrder)
    putStrLn "Goodbye"
