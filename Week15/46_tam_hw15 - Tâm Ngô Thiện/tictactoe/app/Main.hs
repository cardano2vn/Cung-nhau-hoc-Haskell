module Main where

import Matrix
import Tictactoe

main :: IO ()
main = do
    putStrLn "Let's play."
    playWith2PlayersWrapper
    -- playWithMachineWrapper
