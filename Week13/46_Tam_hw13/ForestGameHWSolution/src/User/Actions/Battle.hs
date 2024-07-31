{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module User.Actions.Battle where

import System.Random (randomRIO)

data Golem = Golem { gAttack :: Int, gHp :: Int } deriving Show
data Player = Player { pAttack :: Int, pHp :: Int } deriving (Show)
data Battle = Fight | RunAway deriving (Show, Read, Eq)

battleBackend :: Golem -> Player -> IO Bool
battleBackend golem player = do
    putStrLn "--------------------------------------------------"
    putStrLn "Choose Fight or RunAway"
    putStrLn $ "Golem has " ++ show (gHp golem) ++ " hp, " ++ show (gAttack golem) ++ " attack points"
    putStrLn $ "Player has " ++ show (pHp player) ++ " hp, " ++ show (pAttack player) ++ " attack points"
    inputStr <- getLine
    let input = read @Battle inputStr

    if input == Fight then do
        golemLuckFactor <- randomRIO @Int (1, 3)
        playerLuckFactor <- randomRIO @Int (1, 3)
        putStrLn $ "golemLuckFactor: " ++ show golemLuckFactor
        putStrLn $ "playerLuckFactor: " ++ show playerLuckFactor

        let newPlayer = Player {pHp = pHp player - golemLuckFactor * gAttack golem, pAttack = pAttack player}
        let newGolem = Golem {gHp = gHp golem - playerLuckFactor * pAttack player, gAttack = gAttack golem}
        if pHp newPlayer <= 0 then do
            putStrLn "you are dead"
            return False
        else if gHp newGolem <= 0 then do
            putStrLn "you win"
            return True
        else
            battleBackend newGolem newPlayer
    else do
        golemLuckFactor <- randomRIO @Int (0, 3)
        -- 0 means player is in luck, can run away
        -- others mean the same as golemLuckFactor

        if golemLuckFactor == 0 then do
            putStrLn "you have escapted"
            return True
        else do
            putStrLn "Failed to escape"
            putStrLn $ "golemLuckFactor: " ++ show golemLuckFactor
            let newPlayer = Player {pHp = pHp player - golemLuckFactor * gAttack golem, pAttack = pAttack player}
            battleBackend golem newPlayer

    
battle :: IO Bool
battle = do
    gAttack <- randomRIO @Int (5, 10)
    pAttack <- randomRIO @Int (5, 10)
    gHp <- randomRIO @Int (20, 100)
    pHp <- randomRIO @Int (20, 100)
    let golem = Golem {gAttack, gHp}
    let player = Player {pAttack, pHp}

    putStrLn "You have encountered a golem"
    battleBackend golem player