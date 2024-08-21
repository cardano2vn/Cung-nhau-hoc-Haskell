module Games.Hangman where

import Data.Char
import Data.List.Split ( chunksOf )
import System.Console.ANSI
import System.Random ( randomRIO )
import Utils.Helpers 
import Utils.GameSetting

hangmanStages :: [String]
hangmanStages =
    [ "    ___\n   |  \\|\n       |\n       |\n       |\n       |\n     __|__\n"
    , "    ___\n   |  \\|\n   o   |\n       |\n       |\n       |\n     __|__\n"
    , "    ___\n   |  \\|\n   o   |\n   +   |\n       |\n       |\n     __|__\n"
    , "    ___\n   |  \\|\n   o   |\n  -+   |\n       |\n       |\n     __|__\n"
    , "    ___\n   |  \\|\n   o   |\n  -+-  |\n       |\n       |\n     __|__\n"
    , "    ___\n   |  \\|\n   o   |\n  -+-  |\n  /    |\n       |\n     __|__\n"
    , "    ___\n   |  \\|\n   o   |\n  -+-  |\n  / \\  |\n       |\n     __|__\n"
    ]

chooseWord :: IO String
chooseWord = do
    contents <- readFile wordsFile
    let wordsList = lines contents
    index <- randomRIO (0, length wordsList - 1)
    return (wordsList !! index)

displayWord :: String -> [Char] -> String
displayWord word guesses = [if c `elem` guesses then c else '-' | c <- word]

displayAlphabet :: [Char] -> String
displayAlphabet guesses =
    let alphabet = ['a'..'z']
        chunks = chunksOf 9 alphabet
        markLetter c = if c `elem` map toLower guesses then '[' : c : "] " else " " ++ [c] ++ "  "
    in unlines $ map (concatMap markLetter) chunks

gameLoop :: String -> [Char] -> Int -> Either String String -> IO GameState
gameLoop word guesses remainingGuesses message = do
    clearScreen
    setCursorPosition 0 0
    
    putStrLn $ boldText "Hangman\n"
    
    -- thông báo cho biết ký tự được chọn có đúng ko
    case message of
        Left msg -> setSGR [SetColor Foreground Dull Red] >> putStrLn msg
        Right msg -> setSGR [SetColor Foreground Dull Green] >> putStrLn msg
    setSGR [Reset]

    putStrLn $ hangmanStages !! (length hangmanStages - remainingGuesses - 1)
    putStr $ boldText "Word: "
    setSGR [SetColor Background Vivid Yellow, SetColor Foreground Vivid Black]
    putStr (boldText $ displayWord word guesses)
    setSGR [Reset]
    putStrLn "\n"
    setSGR [SetColor Foreground Vivid Blue]
    putStr $ displayAlphabet guesses
    setSGR [Reset]
    putStr "\nRemaining guesses: "
    setSGR [SetColor Foreground Vivid Yellow]
    print remainingGuesses
    setSGR [Reset]

    if remainingGuesses == 0 then do
        setSGR [SetColor Background Vivid Red]
        putStrLn $ "Game over! You've been hanged! The word was: " ++ word
        setSGR [Reset]
        return Finish
    else if all (`elem` guesses) word then do
        setSGR [SetColor Background Vivid Green]
        putStrLn "Congratulations! You've guessed the word!"
        setSGR [Reset]
        return Finish
    else do
        putStr "Enter your guess. "
        input <- getCharAndCheckEsc
        case input of
            Nothing -> return Exit
            Just guess -> do
                if not (isAlpha guess) then
                    gameLoop word guesses remainingGuesses (Left $ guess : "\tPlease enter an alphabet letter.")
                else
                    let guessedLetter = toLower guess
                    in if guessedLetter `elem` map toLower guesses then
                        gameLoop word guesses remainingGuesses (Left $ guess : "\tYou've already guessed that letter.")
                    else if guessedLetter `elem` word then
                        gameLoop word (guessedLetter : guesses) remainingGuesses (Right $ guess : "\tGood guess!")
                    else
                        gameLoop word (guessedLetter : guesses) (remainingGuesses - 1) (Left $ guess : "\tWrong guess!")

hangman :: IO GameState
hangman = do
    word <- chooseWord
    gameLoop word [] (length hangmanStages - 1) (Right "")