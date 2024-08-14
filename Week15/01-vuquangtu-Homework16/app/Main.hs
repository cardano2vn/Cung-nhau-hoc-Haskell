module Main where

import           Actions.CheckWinner
import           HelperFunc.Helper
import           User.Player
import           User.Types

main :: IO ()
main = do
  putStrLn "Welcome to Tic Tac Toe Game"
  player1Name <- prompt "Enter name of player 1?"
  putStrLn $ "Hello " ++ player1Name ++ ", your marker is: X"
  option <- prompt "What do you choose?\n1 - Player by hand\n2 - Auto Player?"
  case option of
    "1" -> prompt "Enter name of player 2?" >>= setupGame player1Name
    "2" -> setupGame player1Name "autoPlayer"
    _   -> putStrLn "Invalid option. Please enter 1 or 2." >> main

setupGame :: String -> String -> IO ()
setupGame player1Name player2Name = do
  putStrLn $ "Hello " ++ player2Name ++ ", your marker is: O"
  gameLoop [] (player1Name, player2Name) "player1"

gameLoop :: Moves -> (String, String) -> String -> IO ()
gameLoop moves (player1Name, player2Name) currentPlayer = do
  newMoves <-
    player
      moves
      currentPlayer
      (if currentPlayer == "player1"
         then player1Name
         else player2Name)
  case checkWinner (playGame newMoves) of
    Just winner ->
      putStrLn
        $ "The winner is: "
            ++ if winner == X
                 then player1Name
                 else player2Name
    Nothing ->
      gameLoop
        newMoves
        (player1Name, player2Name)
        (nextPlayer currentPlayer player2Name)

nextPlayer :: String -> String -> String
nextPlayer "player1" "autoPlayer" = "autoPlayer"
nextPlayer "player1" _            = "player2"
nextPlayer _ _                    = "player1"
