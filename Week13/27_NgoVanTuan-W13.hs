module User.Actions.Battle where

import System.Random (randomRIO)
import Text.Read (readMaybe)

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


showStatus :: Golem -> Player -> IO ()
showStatus Golem { gHp = gHp, gAttack = gAttack } Player { pHp = pHp, pAttack = pAttack } = do
  putStrLn $ "\nGolem: " ++ show gHp ++ " HP, " ++ show gAttack ++ " Attack"
  putStrLn $ "Player: " ++ show pHp ++ " HP, " ++ show pAttack ++ " Attack"
  putStrLn "Chon hanh dong: Fight hoac RunAway?"


attemptRunAway :: Golem -> Player -> IO Bool
attemptRunAway golem@Golem {..} player@Player {..} = do
  escapeChance <- randomRIO @Int (1, 3)
  damage <- randomRIO @Int (10, 20)
  let newPlayerHp = pHp - damage
  if escapeChance == 2
    then putStrLn "Ban da chay thoat thanh cong!" >> return True
    else do
      putStrLn $ "Ban khong the chay thoat va bi Golem tan cong! Mat " ++ show damage ++ " HP."
      if newPlayerHp <= 0
        then putStrLn "Ban da thua truoc Golem." >> return False
        else showStatus golem player { pHp = newPlayerHp } >> playerAction golem player { pHp = newPlayerHp }

playerAction :: Golem -> Player -> IO Bool
playerAction golem@Golem {..} player@Player {..} = do
  action <- getLine
  case readMaybe action of
    Just Fight -> do
      damageToGolem <- randomRIO @Int (15, 25)
      damageToPlayer <- randomRIO @Int (10, 20)
      let newGolemHp = gHp - damageToGolem
          newPlayerHp = pHp - damageToPlayer
      putStrLn $ "Ban gay " ++ show damageToGolem ++ " sat thuong len Golem."
      putStrLn $ "Golem gay " ++ show damageToPlayer ++ " sat thuong len ban."
      showStatus golem { gHp = newGolemHp } player { pHp = newPlayerHp }
      if newPlayerHp <= 0
        then putStrLn "Ban da thua tran!" >> return False
        else if newGolemHp <= 0
               then putStrLn "Ban da thang tran!" >> return True
               else playerAction golem { gHp = newGolemHp } player { pHp = newPlayerHp }
    Just RunAway -> attemptRunAway golem player
    Nothing -> do
      putStrLn "Hanh dong khong hop le. Vui long chon lai: Fight hoac RunAway."
      playerAction golem player

battle :: IO Bool
battle = do
  putStrLn "\nBan da gap phai mot Golem! Chon hanh dong: Fight hoac RunAway?"
  gAttack <- randomRIO @Int (4, 10)
  gHp <- randomRIO @Int (30, 50)
  pAttack <- randomRIO @Int (4, 10)
  pHp <- randomRIO @Int (30, 50)
  let golem = Golem { gAttack, gHp }
      player = Player { pAttack, pHp }
  showStatus golem player
  playerAction golem player
