module Main where

import System.IO

import Hangman
import Game2048

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering -- don't wait to hit enter when input char
    select 0

showSelections :: Int -> String
showSelections 0 = "> Hangman \n  Game 2048\n  Quit"
showSelections 1 = "  Hangman \n> Game 2048\n  Quit"
showSelections 2 = "  Hangman \n  Game 2048\n> Quit"

select :: Int -> IO()
select i = do
    putStrLn $ showSelections i
    chr <- getChar
    putStrLn ""
    if chr == 'w' then (if i == 0 then select 2 else select (i - 1))
    else if chr == 's' then select $ (i + 1) `mod` 3
    else if chr == toEnum 10 then do -- press enter
        if i == 0 then do
            Hangman.playHangman
            select i
        else if i == 1 then do
            Game2048.play2048
            select i
        else
            putStrLn "Exit"
    else do
        select i
