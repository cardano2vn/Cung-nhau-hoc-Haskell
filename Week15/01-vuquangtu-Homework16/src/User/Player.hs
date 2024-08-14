module User.Player where

import           HelperFunc.Helper

import           System.Random     (randomRIO)
import           User.Types

player :: Moves -> String -> String -> IO Moves
player moves option name =
  let playerProcess playerMoveR playerMoveC = do
        let move =
              ( if option == "player1"
                  then X
                  else O
              , (read playerMoveR, read playerMoveC)) :: (Player, Location)
            newMoves = move : moves
        putStrLn $ showBoard newMoves
        return newMoves
   in if option == "autoPlayer"
        then do
          playerMoveR <- randomRIO (0, 10) :: IO Int
          playerMoveC <- randomRIO (0, 10) :: IO Int
          playerProcess (show playerMoveR) (show playerMoveC)
        else do
          playerMoveR <- prompt $ "Enter " ++ name ++ "'s row of moving"
          playerMoveC <- prompt $ "Enter " ++ name ++ "'s column of moving"
          playerProcess playerMoveR playerMoveC
