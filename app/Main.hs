module Main
    ( main
    ) where
import           Algs                (methodsToEnforce, plan)
import           Control.Monad
import           Control.Monad.State
import           Data.Foldable       (traverse_)
import           Data.Map            (Map)
import qualified Data.Map            as Map
import           HotDrink            (Constraint (..), methodToGraph)
import           MethodParser        (Expr (Var), Parser (runParser), expr)
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
        ["readVars"] -> do
            vars <- gets variables
            liftIO $ putStrLn $ "Variables: " ++ show vars
        ["readCons"] -> do
            cons <- gets constraints
            liftIO $ putStrLn $ "Constraints: " ++ show cons
        ["readStay"] -> do
            stay <- gets strength
            liftIO $ putStrLn $ "Stay variables: " ++ show stay
        ["plan"] -> do
            cons <- gets constraints
            st <- gets strength
            -- map st to a list of constraints with methodgraphs that points from a method with the expr (Var name of st element) to the variable node with the name of the st element
            let order = map (\s -> Constraint [methodToGraph [] s ("m" ++ s, [(s, Var s)])]) st
                methods = methodsToEnforce (plan order $ mconcat cons)
            case methods of
                Just m  -> liftIO $ putStrLn $ "Plan: " ++ show m
                Nothing -> liftIO $ putStrLn "No plan found"
        ["exit"] -> return ()
        _ -> liftIO $ putStrLn "Unknown command"

inputMethod :: StateT ConstraintSystem IO ()
inputMethod = do
    liftIO $ putStrLn "Enter name of method:"
    name <- liftIO getLine
    liftIO $ putStrLn "Enter space separated input names to method:"
    inputsStr <- liftIO getLine
    liftIO $ putStrLn "Enter output variable to method:"
    outputStr <- liftIO getLine
    liftIO $ putStrLn "Enter method body:"
    bodyStr <- liftIO getLine
    let inputs = words inputsStr
        output = outputStr
        methodBody = runParser (expr :: Parser Char String Expr) bodyStr
    case methodBody of
        Right (e, "") -> do
            -- check that inputs and outputs are in the variables map
            cs <- get
            liftIO $ print inputs
            liftIO $ putStrLn output
            if all (\i -> Map.member i (variables cs)) inputs && Map.member output (variables cs)
                then do
                    liftIO $ putStrLn "Parse success"
                    let method = (name, [(output, e)])
                        methodGraph = methodToGraph inputs output method
                    modify $ \s -> s { constraints = Constraint (methodGraph : unConstraint (head $ constraints s)) : drop 1 (constraints s) }
                else
                    liftIO $ putStrLn "Parse fail: input or output not in variables"
        Right (_, trail) ->
            liftIO $ putStrLn $ "Parse fail at: " ++ trail
        Left e ->
            liftIO $ putStrLn $ "Parse fail: " ++ show e

prompt :: IO String
prompt = do
    putStr "$ "
    hFlush stdout
    getLine

userInputLoop :: StateT ConstraintSystem IO ()
userInputLoop = do
    input <- liftIO prompt
    processInput input
    unless (input == "exit") userInputLoop

main :: IO ()
main = do
    putStrLn "Welcome to HotDrink"
    evalStateT userInputLoop (ConstraintSystem Map.empty [] [])
    putStrLn "Goodbye"























    {-
data State
  = State
      { variables   :: [Variable]
      , constraints :: [Constraint]
      }
  deriving (Eq, Show)

inputMethod :: State -> IO ()
inputMethod (State vars cs) = do
    putStrLn "Enter name of method:"
    name <- getLine
    putStrLn "Enter comma separated input names to method:"
    inputsStr <- getLine
    putStrLn "Enter output variable to method:"
    outputStr <- getLine
    putStrLn "Enter method body:"
    bodyStr <- getLine
    let inputs = words inputsStr
        output = outputStr
        methodBody = MethodParser.runParser (MethodParser.expr :: Parser Char String Expr) bodyStr
    case methodBody of
        Right (e, _) -> do
            putStrLn "Parse success"
            let method = (name, [(output, e)])
                methodGraph = methodToGraph inputs output method
            userInputLoop (State vars (Constraint (methodGraph : unConstraint (head cs)) : drop 1 cs))
        Left _ ->
            putStrLn "Parse fail"

userInputLoop :: State -> IO ()
userInputLoop (State vars cs) = do
    putStr "Current state: "
    print $ "Vars: " ++ show vars
    print $ "Constraints: " ++ show cs
    putStr "$ "
    inp <- getLine
    case inp of
      "addVar" -> do
          putStrLn "Enter variable name"
          putStr "$ "
          var <- getLine
          putStrLn "Enter value"
          putStr "$ "
          valStr <- getLine
          case reads valStr of
              [(val, "")] -> do
                  userInputLoop (State ((var, Just val) : vars) cs)
              _ -> do
                  putStrLn "Couldnt parse the value"
                  userInputLoop (State vars cs)
      "addCons" -> do
          putStrLn "Enter number of methods in constraint:"
          nMethodsStr <- getLine
          case reads nMethodsStr of
              [(n, "")] -> do
                  traverse_ (\_ -> inputMethod (State vars (Constraint [] : cs))) [1..n]
              _ -> do
                  putStrLn "Couldnt parse the number of methods"
                  userInputLoop (State vars cs)



      _ -> do
          putStrLn "Unknown command"
          userInputLoop (State vars cs)


main :: IO ()
main = userInputLoop (State [] [])
-}

-- main :: IO ()
-- main = userInput Init
