module Hangman where

import System.Random ( randomRIO )
import Data.Char ( toLower )

-- Function to randomly select a word from a file
getRandomWord :: IO String
getRandomWord = do
  contents <- readFile "./src/hangman/hangman_words.txt"
  let wordsList = lines contents
  indexW <- randomRIO (0, length wordsList - 1)
  return $ wordsList !! indexW

-- Function to display the game state
displayGameState :: String -> Int -> [Char] -> IO ()
displayGameState word guesses guessedLetters = do
  -- putStrLn   " ___"
  -- putStrLn   "|  \\|"
  -- putStrLn   "    |"
  -- putStrLn   "    |"
  -- putStrLn   "    |"
  -- putStrLn   "    |"
  -- putStrLn   "  __|__ \n"
  putStrLn $ "Word: " ++ map (\c -> if c `elem` guessedLetters then c else '_') word
  putStrLn $ "Remaining guesses: " ++ show guesses
  putStrLn $ "Guessed letters: " ++ show guessedLetters

-- Main game loop
playHangman :: String -> IO ()
playHangman word = do
  let maxGuesses = 6
      wordLength = length word
      guessedLetters = []
      hiddenWord = replicate wordLength '_'

  displayGameState hiddenWord maxGuesses guessedLetters

  loop hiddenWord maxGuesses guessedLetters
  where
    loop hiddenWord guesses guessedLetters = do
      putStrLn "Enter your guess: "
      guess <- getChar
      let newGuessedLetters = guess : guessedLetters
          isCorrect = guess `elem` word
      if isCorrect 
        then do
          let newHiddenWord = zipWith (\c g -> if c == guess then c else g) word hiddenWord
          displayGameState newHiddenWord guesses newGuessedLetters
          if newHiddenWord == word
            then putStrLn "Congratulations! You've guessed the word!"
                  >> playAgain
            else loop newHiddenWord guesses newGuessedLetters
        else do
          putStrLn $ show guess ++ "   Wrong guess!"
          let newGuesses = guesses - 1          
          displayGameState hiddenWord newGuesses newGuessedLetters
          if newGuesses == 0
            then putStrLn "Game over! You've been hanged!" 
                  >> putStrLn ("The exact word is " ++ "'" ++ show word ++ "'") 
                  >> playAgain
            else do loop hiddenWord newGuesses newGuessedLetters

playAgain :: IO ()
-- ... prompt user to play again
playAgain = do
  putStrLn "Do you want to play again? (y/n): "
  answer <- getChar
  if toLower answer == 'y'
    then do 
      word <- getRandomWord
      playHangman word
    else return ()