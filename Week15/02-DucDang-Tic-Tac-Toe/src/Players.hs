module Players where

import Actions

singlePlayerGame :: IO ()
singlePlayerGame = do
  let initialPlayer = X
      opponent = O
      board = initialBoard
  putStrLn "You are X, the bot is O"
  playGame initialPlayer opponent board

multiPlayerGame :: IO ()
multiPlayerGame = do
  let initialPlayer = X
      opponent = O
      board = initialBoard
  putStrLn "Player 1 is X, Player 2 is O"
  playGame initialPlayer opponent board