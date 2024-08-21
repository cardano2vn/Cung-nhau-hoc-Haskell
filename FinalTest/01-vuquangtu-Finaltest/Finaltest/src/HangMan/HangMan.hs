module HangMan.HangMan where

import           System.IO           (hFlush, stdout)
import           System.Random       (randomRIO)

import           System.Console.ANSI (Color (..), ColorIntensity (..),
                                      ConsoleLayer (..), SGR (..),
                                      clearFromCursorToScreenEnd, clearScreen,
                                      setCursorPosition)
import           Utility.Color

readSecretWord :: FilePath -> IO String
readSecretWord filePath = do
  contents <- readFile filePath
  let wordsList = lines contents
  idx <- randomRIO (0, length wordsList - 1)
  return (wordsList !! idx)

displayWord :: String -> [Char] -> String
displayWord word guesses =
  map
    (\c ->
       if c `elem` guesses
         then c
         else '-')
    word

displayHangman :: Int -> IO ()
displayHangman wrongGuesses = do
  let hangmanStates =
        [ "    ___\n   |  \\|\n       |\n       |\n       |\n       |\n     __|__"
        , "    ___\n   |  \\|\n   o   |\n       |\n       |\n       |\n     __|__"
        , "    ___\n   |  \\|\n   o   |\n   |   |\n       |\n       |\n     __|__"
        , "    ___\n   |  \\|\n   o   |\n  -+   |\n       |\n       |\n     __|__"
        , "    ___\n   |  \\|\n   o   |\n  -+-  |\n       |\n       |\n     __|__"
        , "    ___\n   |  \\|\n   o   |\n  -+-  |\n  /    |\n       |\n     __|__"
        , "    ___\n   |  \\|\n   o   |\n  -+-  |\n  / \\  |\n       |\n     __|__"
        ]
  putStrLn (hangmanStates !! wrongGuesses)

displayGuessedLetters :: String -> IO ()
displayGuessedLetters guesses = do
  let alphabet = ['a' .. 'z']
  putStrLn
    $ concatMap
        (\c ->
           if c `elem` guesses
             then '[' : c : "] "
             else c : " ")
        alphabet

displayResultGuessed :: String -> String -> Char -> IO ()
displayResultGuessed word guesses c = do
  setCursorPosition 2 0
  clearFromCursorToScreenEnd
  putStrLn
    $ case () of
        _
          | c `notElem` ['a' .. 'z'] ->
            setSGRCode [SetColor Foreground Vivid Yellow]
              ++ [c]
              ++ "       Please enter an alphabet letter.\n"
              ++ setSGRCode [Reset]
          | c `elem` guesses && c `elem` word ->
            setSGRCode [SetColor Foreground Vivid Blue]
              ++ [c]
              ++ "       You've already guessed that letter.\n"
              ++ setSGRCode [Reset]
          | c `notElem` word ->
            setSGRCode [SetColor Foreground Vivid Red]
              ++ [c]
              ++ "       Wrong guess!\n"
              ++ setSGRCode [Reset]
          | otherwise ->
            setSGRCode [SetColor Foreground Vivid Green]
              ++ [c]
              ++ "       Good guess!\n"
              ++ setSGRCode [Reset]

playHangman :: String -> [Char] -> Int -> IO ()
playHangman word guesses remainingGuesses = do
  displayHangman (6 - remainingGuesses)
  putStrLn $ "\nWord: " ++ displayWord word guesses ++ "\n"
  displayGuessedLetters guesses
  putStrLn
    $ setSGRCode [SetColor Foreground Vivid Cyan]
        ++ "\nRemaining guesses: "
        ++ show remainingGuesses
        ++ "\n"
        ++ setSGRCode [Reset]
  if displayWord word guesses == word
    then do
      putStrLn "Congratulations! You've guessed the word!"
      playAgain
    else if remainingGuesses <= 0
           then do
             putStrLn $ "Game over! You've been hanged! The word was: " ++ word
             hFlush stdout
             playAgain
           else do
             putStr "Enter your guess: \n"
             hFlush stdout
             guess <- getChar
             case guess of
               '\ESC' -> return ()
               _ -> do
                 let newGuesses = guess : guesses
                 let newRemainingGuesses =
                       if guess `elem` word
                         then remainingGuesses
                         else remainingGuesses - 1
                 displayResultGuessed word guesses guess
                 playHangman word newGuesses newRemainingGuesses

playAgain :: IO ()
playAgain = do
  putStr "Do you want to play again? (y/n): "
  hFlush stdout
  response <- getChar
  clearScreen
  setCursorPosition 0 0
  if response == 'y'
    then mainHangMan
    else putStrLn "Thanks for playing!"

mainHangMan :: IO ()
mainHangMan = do
  clearScreen
  setCursorPosition 0 0
  putStrLn
    $ setSGRCode [SetColor Foreground Vivid Yellow]
        ++ "HANG MAN GAME"
        ++ setSGRCode [Reset]
  secretWord <- readSecretWord "hangman_words.txt"
  playHangman secretWord [] 6
