module Main
    ( main
    ) where
import           CLI                 (userInputLoop)
import           Control.Monad.State (evalStateT)
import           WarmDrinkF          (ConstraintSystem (..))

--- Main function (entry point) ---

main :: IO ()
main = do
    putStrLn "Welcome to HotDrink"
    putStrLn "Type 'help' for a list of commands"
    evalStateT userInputLoop (ConstraintSystem [] [])
    putStrLn "Goodbye"
