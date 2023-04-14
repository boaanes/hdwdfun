module Main
    ( main
    ) where
import           Data.Foldable (traverse_)
import           HotDrink      (Constraint (..), Variable, methodToGraph)
import           MethodParser  (Expr, Parser (runParser), expr)

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

-- main :: IO ()
-- main = userInput Init
