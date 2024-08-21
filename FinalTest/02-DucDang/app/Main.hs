module Main where

import Hangman ( getRandomWord, playHangman )

main :: IO ()
main = do
  putStrLn "1. Hangman"
  putStrLn "2. 2048"
  putStrLn "3. Quit"
  opt <- getLine
  case read opt :: Int of
    1 -> do
      putStrLn "Welcome to the Hangman Game"
      word <- getRandomWord
      playHangman word
    2 -> undefined
    3 -> putStrLn "Thanks for playing! Goodbye."
    _ -> putStrLn "Invalid choice"