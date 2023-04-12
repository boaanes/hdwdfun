module UserInput
    ( InputState (..)
    , userInput
    ) where
import           Algs
import           Control.Monad.State
import           Data.Char           (toLower)
import           Data.Maybe          (fromMaybe)
import           HotDrink
import qualified Whap

data InputState = Init | Prompt | Exit | Plan | Help | Unknown deriving
    ( Eq
    , Show
    )

instance Read InputState where
    readsPrec _ "init"   = [(Init, "")]
    readsPrec _ "prompt" = [(Prompt, "")]
    readsPrec _ "exit"   = [(Exit, "")]
    readsPrec _ "plan"   = [(Plan, "")]
    readsPrec _ "help"   = [(Help, "")]
    readsPrec _ _        = [(Unknown, "")]

handleInput :: (InputState -> IO ()) -> InputState -> IO ()

handleInput nextInput Init = do
    putStrLn "Welcome to the HotDrink program"
    nextInput Prompt

handleInput nextInput Prompt = do
    putStr "$ "
    inp <- getLine
    nextInput $ read (map toLower inp)

handleInput _ Exit = do
    putStrLn "Exiting..."

handleInput nextInput Help = do
    putStrLn "List of available commands:"
    putStrLn $ replicate 40 '-'
    putStrLn "help - displays this help message"
    putStrLn "exit - exits the program"
    putStrLn $ replicate 40 '-'
    nextInput Prompt

handleInput nextInput Plan = do
    let initialState = [("area", Just 3600.0), ("perimeter", Just 40.0), ("width", Just 10.0), ("height", Just 10.0)] :: [Variable]
        p = plan [Whap.stayArea, Whap.stayPerimeter, Whap.stayWidth, Whap.stayHeight] (mconcat [Whap.constraintA, Whap.constraintB])
        methods = methodsToEnforce p
        exprs = concatExprsInMethodList <$> methods
    case exprs of
        Just _ -> do
            (_, finalState) <- runStateT (traverse updateVariable (fromMaybe [] exprs)) initialState
            putStrLn $ "Final state: " ++ show finalState
        Nothing -> putStrLn "No methods to enforce"
    nextInput Prompt

handleInput nextInput Unknown = do
    putStrLn "Unknown command"
    nextInput Prompt

userInput :: InputState -> IO ()
userInput = handleInput userInput
