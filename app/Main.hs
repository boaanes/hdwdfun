module Main
    ( main
    ) where
import qualified Agenda
import           CLI                 (userInputLoop)
import           Control.Monad.State (evalStateT)
import           WarmDrinkF          (ConstraintSystem (..))

--- Main function (entry point) ---

main :: IO ()
main = do
    putStrLn "Welcome to ScaldishDrink!"
    putStrLn "Type 'help' for a list of commands"
    evalStateT userInputLoop (ConstraintSystem [Agenda.compA, Agenda.compB] [Agenda.inter])
    putStrLn "Goodbye"
