module Main where

import           Forest.Level1       (Forest (..), level1forest)
import           System.Random       (randomRIO)
import           Text.Read           (readMaybe)
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
            selectedMove <- getLine
            case readMaybe @AvailableMoves selectedMove of
              Just move' -> gameLoop $ move (s, forest) move'
              Nothing ->
                putStrLn
                  "Invalid move ,Choose a path, brave adventurer: GoLeft, GoRight, or GoForward."
                  >> continueLoop
      battleDice <- randomRIO @Int (1, 2)
      case battleDice of
        2 -> do
          r <- battle
          if r
            then continueLoop
            else return ()
        _ -> continueLoop
