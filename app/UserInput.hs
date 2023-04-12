module UserInput
    ( userInput
    ) where
import           Algs
import           Control.Monad.State
import           Data.Maybe          (fromMaybe)
import           HotDrink
import qualified Whap

handleInput :: (String -> IO ()) -> String -> IO ()

handleInput nextInput "start" = do
    putStrLn "Welcome to the HotDrink program"
    nextInput "prompt"

handleInput nextInput "prompt" = do
    putStr "$ "
    inp <- getLine
    nextInput inp

handleInput _ "exit" = do
    putStrLn "Exiting..."

handleInput nextInput "help" = do
    putStrLn "List of available commands:"
    putStrLn $ replicate 40 '-'
    putStrLn "help - displays this help message"
    putStrLn "exit - exits the program"
    putStrLn $ replicate 40 '-'
    nextInput "prompt"

handleInput nextInput "plan" = do
    let initialState = [("area", Just 3600.0), ("perimeter", Just 40.0), ("width", Just 10.0), ("height", Just 10.0)] :: [Variable]
        p = plan [Whap.stayArea, Whap.stayPerimeter, Whap.stayWidth, Whap.stayHeight] (mconcat [Whap.constraintA, Whap.constraintB])
        methods = methodsToEnforce p
        exprs = concatExprsInMethodList <$> methods
    case exprs of
        Just _ -> do
            (_, finalState) <- runStateT (traverse updateVariable (fromMaybe [] exprs)) initialState
            putStrLn $ "Final state: " ++ show finalState
        Nothing -> putStrLn "No methods to enforce"
    nextInput "prompt"

handleInput nextInput input = do
    putStrLn $ "Unknown command: '" ++ input ++ "'"
    nextInput "prompt"

userInput :: String -> IO ()
userInput = handleInput userInput
