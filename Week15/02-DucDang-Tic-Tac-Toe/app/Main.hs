module Main where

import Players ( multiPlayerGame, singlePlayerGame )

main :: IO ()
main = do
  putStrLn "Welcome toTic-Tac-Toe \n Please select a mode"
  putStrLn "1. Single Player"
  putStrLn "2. Multiple Players"
  choice <- readLn
  case choice of 
    1 -> singlePlayerGame
    2 -> multiPlayerGame
    _ -> putStrLn "Invalid Option" >> main

-- Note: I don't handle the 'checkWin' function in Actions yet.