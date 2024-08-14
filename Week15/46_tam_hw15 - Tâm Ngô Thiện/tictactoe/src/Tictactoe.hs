{-# LANGUAGE TypeApplications #-}

module Tictactoe where

import qualified Data.Map
import System.Random (randomRIO)

import Matrix

globalWinSize :: Int
globalWinSize = 3

-- 2 PLAYERS

playWith2Players :: Matrix -> Int -> IO()
playWith2Players matrix player = do
    putStrLn $ "Player" ++ show player ++ "'s turn, your mark is " ++ show (getMark player)
    content <- getLine 
    let position = parseInput content :: Maybe (Int, Int)
    case position of 
        Nothing -> do
            putStrLn "Cannot parsed input. Enter again"
            playWith2Players matrix player
        Just pos ->
            if not $ isValidInput pos then do
                putStrLn "Invalid input. Enter again"
                playWith2Players matrix player
            else if not $ isPositionEmpty matrix pos then do 
                putStrLn "Position is taken. Enter again"
                playWith2Players matrix player
            else do
                let newMatrix = updateMatrix matrix pos (getMark player)
                if checkWin newMatrix pos (getMark player) globalWinSize then do
                    printMatrix newMatrix
                    putStrLn $ "Player " ++ show player ++ " won"
                else do
                    printMatrix newMatrix
                    playWith2Players newMatrix (1 - player)

playWith2PlayersWrapper :: IO()
playWith2PlayersWrapper = playWith2Players Data.Map.empty 0

-- PLAY WITH MACHINE

randomPosition :: Int -> IO Position
randomPosition size = do
    row <- randomRIO @Int (0, size)
    col <- randomRIO @Int (0, size)
    return (row, col)

machinePlay :: Matrix -> IO (Matrix, Position)
machinePlay matrix = do
    pos <- randomPosition (getMatrixSize matrix - 1)
    if isPositionEmpty matrix pos then
        return (updateMatrix matrix pos X, pos)
    else machinePlay matrix

playWithMachine :: Matrix  -> IO()
playWithMachine matrix = do
    putStrLn "Your turn"
    content <- getLine 
    let position = parseInput content :: Maybe (Int, Int)
    case position of 
        Nothing -> do
            putStrLn "Cannot parsed input. Enter again"
            playWithMachine matrix
        Just pos ->
            if not $ isValidInput pos then do
                putStrLn "Invalid input. Enter again"
                playWithMachine matrix
            else if not $ isPositionEmpty matrix pos then do 
                putStrLn "Position is taken. Enter again"
                playWithMachine matrix
            else do
                -- player's turn
                let newMatrix = updateMatrix matrix pos O
                if checkWin newMatrix pos O globalWinSize then do
                    printMatrix newMatrix
                    putStrLn "Player won"
                else do
                    -- machine's turn
                    (newMatrix2, pos2) <- machinePlay newMatrix
                    if checkWin newMatrix2 pos2 X globalWinSize then do
                        printMatrix newMatrix2
                        putStrLn "Machine won"
                    else do
                        printMatrix newMatrix2
                        playWithMachine newMatrix2


playWithMachineWrapper :: IO()
playWithMachineWrapper = playWithMachine Data.Map.empty