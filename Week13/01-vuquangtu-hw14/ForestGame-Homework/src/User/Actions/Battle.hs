
module User.Actions.Battle where

import           System.Random (randomRIO)
import           Text.Read     (readMaybe)

data Golem = Golem
  { gAttack :: Int
  , gHp     :: Int
  } deriving (Show)

data Player = Player
  { pAttack :: Int
  , pHp     :: Int
  } deriving (Show)

data Battle
  = Fight
  | RunAway
  deriving (Show, Read)

displayUser :: Golem -> Player -> IO ()
displayUser Golem {..} Player {..} = do
  if gHp <= 0 || pHp <= 0
    then return ()
    else do
      putStrLn
        $ "\nThe Golem has "
            ++ show gHp
            ++ " Health and "
            ++ show gAttack
            ++ " attack."
      putStrLn
        $ "You have "
            ++ show pHp
            ++ " Health and "
            ++ show pAttack
            ++ " attack."
      putStrLn "Choose an action: Fight or RunAway?"

runAway :: Golem -> Player -> Int -> IO Bool
runAway golem@Golem {..} player@Player {..} minusHealthPlayer = do
  let newpHp = pHp - minusHealthPlayer
      newGAttack = gAttack - 1
  randomLife <- randomRIO @Int (1, 3)
  if randomLife == 2 || newGAttack <= 0
    then putStrLn "You've managed to run away!" >> return True
    else if newpHp >= 0
           then putStrLn "You've failed to run away! And the Golem hit you!"
                  >> displayUser
                       golem {gAttack = newGAttack}
                       player {pHp = newpHp}
                  >> optionResolve
                       golem {gAttack = newGAttack}
                       player {pHp = newpHp}
           else putStrLn "You've lost to the Golem " >> return False

optionResolve :: Golem -> Player -> IO Bool
optionResolve golem@Golem {..} player@Player {..} = do
  option <- getLine
  minusHealthGolem <- randomRIO @Int (40, 40)
  minusHealthPlayer <- randomRIO @Int (20, 30)
  case readMaybe option of
    Just Fight -> do
      let newgHp = gHp - minusHealthGolem
          newpHp = pHp - minusHealthPlayer
          newGAttack = gAttack - 1
          newPAttack = pAttack - 1
      displayUser
        golem {gHp = newgHp, gAttack = newGAttack}
        player {pHp = newpHp, pAttack = newPAttack}
      if newpHp <= 0 || newPAttack <= 0
        then putStrLn "You've lose the battle!" >> return False
        else if newgHp <= 0 || newGAttack <= 0
               then putStrLn "You've won the battle!" >> return True
               else optionResolve
                      golem {gHp = newgHp, gAttack = newGAttack}
                      player {pHp = newpHp, pAttack = newPAttack}
    Just RunAway -> runAway golem player minusHealthPlayer
    Nothing ->
      putStrLn "Choose an action: Fight or RunAway?"
        >> optionResolve golem player

battle :: IO Bool
battle = do
  putStrLn "\nYou've encountered a Golem! Choose an action: Fight or RunAway?"
  gAttack <- randomRIO @Int (4, 10)
  gHp <- randomRIO @Int (40, 60)
  pAttack <- randomRIO @Int (4, 10)
  pHp <- randomRIO @Int (40, 60)
  let golem = Golem {gAttack, gHp}
      player = Player {pAttack, pHp}
  displayUser golem player
  optionResolve golem player
