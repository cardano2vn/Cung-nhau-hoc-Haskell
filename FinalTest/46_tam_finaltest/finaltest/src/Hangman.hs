module Hangman where

import Control.Exception
import Data.Set hiding (null, filter)
import System.Random (randomRIO)

-- prints "example" as "ex-mp-e" while guessing
-- it's also used to check if the guessing word is completed
fillTheWordWithChosenChars :: String -> Set Char -> String
fillTheWordWithChosenChars [] _ = ""
fillTheWordWithChosenChars (x:xs) s = 
    (if member x s then x else '-') : fillTheWordWithChosenChars xs s

getAvailableChars :: Set Char -> String
getAvailableChars s = printBackend ['a'..'z'] s 1
    where
        printBackend :: [Char] -> Set Char -> Int -> String
        printBackend [] _  _= ""
        printBackend (x:xs) s i = (if member x s then "[" ++ [x] ++ "]" else  " " ++ [x] ++ " ") ++
                                  (if i `mod` 9 == 0 then "\n" else "") ++ printBackend xs s (i + 1)

type State = (Int,String,Set Char)

stateToString :: State -> String
stateToString (guessCount, originalWord, chosenChars) = 
    getHangStage guessCount ++ "\n\n" ++
    "Word: " ++ fillTheWordWithChosenChars originalWord chosenChars ++ "\n\n" ++
    getAvailableChars chosenChars ++ "\n\n" ++
    "Remaining guess: " ++ show guessCount

gameHangman :: State -> IO()
gameHangman (_, originalWord, chosenChars) |
    originalWord == fillTheWordWithChosenChars originalWord chosenChars =
    putStrLn $ "You win. The word is: " ++ originalWord

gameHangman (0, originalWord, _) = do
    putStrLn $ "You lose. The word is: " ++ originalWord
    putStrLn $ getHangStage 0

gameHangman state@(guessCount, originalWord, chosenChars) = do
    putStrLn $ stateToString state
    putStrLn "Enter your choice:"
    chr <- getChar
    putStrLn ""
    if chr == toEnum 27 then
        putStrLn "Exit hangman"
    else if chr `notElem` ['a'..'z'] then do
        putStrLn "Invalid choice"
        gameHangman state
    else
        if member chr chosenChars then do
            putStrLn "Char was chosen already"
            gameHangman state
        else do
            if chr `notElem` originalWord then do
                putStrLn "Bad guess"
                gameHangman (guessCount - 1, originalWord, insert chr chosenChars)
            else do
                putStrLn "Good guess"
                gameHangman (guessCount, originalWord, insert chr chosenChars)

playHangman :: IO ()
playHangman = do
    ec <- try $ readFile "hangman_words.txt"
    case ec of
        Left e -> putStrLn $ "exception: " ++ show (e :: IOException)
        Right content ->  do
            let words = lines content
            ranNum <- randomRIO (0, length words - 1) 
            gameHangman (6, words !! ranNum, empty)

-- helper functions to print the man being hanged

getHangStage :: Int -> String
getHangStage i = case i of
    6 -> string6
    5 -> string5
    4 -> string4
    3 -> string3
    2 -> string2
    1 -> string1
    0 -> string0
    _ -> "NOT HANDLED"

string6 = "\
\    ___\n\
\   |  \\|\n\
\       |\n\
\       |\n\
\       |\n\
\       |\n\
\     __|__\n"

string5 = "\
\    ___\n\
\   |  \\|\n\
\   o   |\n\
\       |\n\
\       |\n\
\       |\n\
\     __|__\n"

string4 = "\
\    ___\n\
\   |  \\|\n\
\   o   |\n\
\   +   |\n\
\       |\n\
\       |\n\
\     __|__\n"

string3 = "\
\    ___\n\
\   |  \\|\n\
\   o   |\n\
\  -+   |\n\
\       |\n\
\       |\n\
\     __|__\n"

string2 = "\
\    ___\n\
\   |  \\|\n\
\   o   |\n\
\  -+-  |\n\
\       |\n\
\       |\n\
\     __|__\n"

string1 = "\
\    ___\n\
\   |  \\|\n\
\   o   |\n\
\  -+-  |\n\
\  /    |\n\
\       |\n\
\     __|__\n"

string0 = "\
\    ___\n\
\   |  \\|\n\
\   o   |\n\
\  -+-  |\n\
\  / \\  |\n\
\       |\n\
\     __|__\n"
