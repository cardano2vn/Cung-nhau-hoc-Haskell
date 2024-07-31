{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module User.Actions.Battle where

import System.Random (randomRIO)

data Golem = Golem { gAttack :: Int, gHp :: Int } deriving (Show)
data Player = Player { pAttack :: Int, pHp :: Int } deriving (Show)
data Battle = Fight | RunAway deriving (Show, Read)

battle :: IO Bool
-- battle = undefined
battle = do
    putStrLn "\nUnfortunately! you are facing a beast...\n"
    attackG <- randomRIO @Int (1, 10)
    hpG <- randomRIO @Int (10, 50)
    attackP <- randomRIO @Int (1, 10)
    hpP <- randomRIO @Int (10, 50)
    let
        golem  = Golem  {gAttack = attackG, gHp = hpG} 
        player = Player {pAttack = attackG, pHp = hpG}

    battleLopp golem player    
    where
        battleLopp g p = do
            putStrLn $ "Golem has " ++ show (gAttack g) ++ " dame and " ++ show (gHp g) ++ " healthy"
            putStrLn $ "You have " ++ show (pAttack p) ++ " dame and " ++ show (pHp p) ++ " healthy"
            putStrLn "Fight or RunAway ?"
            result <- getLine
            case read @Battle result of
                Fight -> do
                    luck <- randomRIO @Int (1, 5)
                    let gh = (gHp g) - (pAttack p) * luck
                        ph = (pHp p) - (gAttack g) * luck
                        newG = Golem (gAttack g) gh
                        newP = Player (pAttack p) ph
                    if ph <= 0 
                        then return False -- putStrLn "You died!"
                        else if gh <= 0
                            then return True -- putStrLn "You won"                                 
                            else battleLopp newG newP
                RunAway -> do
                    luck <- randomRIO @Int (1, 5)
                    escapeN <- randomRIO @Int (1, 2)
                    case escapeN of
                        1 -> do
                            let ph = (pHp p) - (gAttack g) * luck
                                newP = Player (pAttack p) ph
                            if ph <= 0
                                then return False --putStrLn "Sadly!, the Beast killed you"                                    
                                else battleLopp g newP
                        2 -> return True
