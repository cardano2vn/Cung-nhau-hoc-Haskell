module Main where

import           Control.Monad       (when)
import           Forest.Level1       (Forest (..), level1forest)
import           System.Random       (randomRIO)
import           User.Actions.Battle
import           User.Actions.Move   (AvailableMoves, move)

main :: IO ()
main = do
  startingStamina <- randomRIO @Int (10_000, 20_000)
  gameLoop (startingStamina, level1forest)
  where
    gameLoop (_, FoundExit) = putStrLn "YOU'VE FOUND THE EXIT!!"
    gameLoop (s, _)
      | s <= 0 = putStrLn "You ran out of stamina and died -.-!"
    gameLoop (s, forest) = do
      putStrLn
        "You're Traped in a Forest , try to scape ! Remember that you lose stamina with each step  you take ."
      let continueLoop = do
            putStrLn
              $ "\nYou have "
                  ++ show s
                  ++ " stamina, and you're still inside the Forest. Choose a path, brave adventurer: GoLeft, GoRight, or GoForward."
            putStrLn "\n1.GoLeft\n2.GoRight\n3.GoForward"
            selectedMove <- getLine
            case selectedMove of
              "1" -> gameLoop $ move (s, forest) (read @AvailableMoves "GoLeft")
              "2" ->
                gameLoop $ move (s, forest) (read @AvailableMoves "GoRight")
              "3" ->
                gameLoop $ move (s, forest) (read @AvailableMoves "GoForward")
              _ ->
                putStrLn
                  "Invalid move ,Choose a path, brave adventurer: GoLeft, GoRight, or GoForward."
                  >> continueLoop
      battleDice <- randomRIO @Int (1, 2)
      case battleDice of
        2 -> do
          r <- battle
          when r continueLoop
        _ -> continueLoop
