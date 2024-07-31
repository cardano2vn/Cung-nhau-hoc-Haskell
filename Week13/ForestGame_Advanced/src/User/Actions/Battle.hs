{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant return" #-}

-- {-# LANGUAGE InstanceSigs #-}
module User.Actions.Battle where

import           Control.Exception (SomeException, try)
import           Data.Maybe        (catMaybes, fromMaybe)
import           System.Directory  (removeFile, renameFile)
import           System.IO         (hClose, openTempFile)
import           System.Random     (randomRIO)
import           Text.Read         (readMaybe)

data GolemType
  = Fire
  | Ice
  | Earth
  deriving (Show, Read, Enum, Bounded)

data Golem = Golem
  { gType   :: GolemType
  , gAttack :: Int
  , gHp     :: Int
  } deriving (Show)

data Player = Player
  { pAttack     :: Int
  , pHp         :: Int
  , pDefense    :: Int
  , pComboCount :: Int
  , pWeapon     :: Maybe Weapon
  , pArmor      :: Maybe Armor
  , pMoney      :: Int
  } deriving (Show, Read, Eq)

data WeaponType
  = Sword
  | Axe
  | Bow
  deriving (Show, Read, Eq)

data ArmorType
  = Shield
  | Helmet
  | Chestplate
  deriving (Show, Read, Eq)

data Weapon = Weapon
  { wType      :: WeaponType
  , wAttack    :: Int
  , wPrice     :: Int
  , wSellPrice :: Int
  } deriving (Show, Read, Eq)

data Armor = Armor
  { aType      :: ArmorType
  , aDefense   :: Int
  , aPrice     :: Int
  , aSellPrice :: Int
  } deriving (Show, Read, Eq)

data ItemType
  = HealingPotion
  | Enhancement
  deriving (Show, Read, Eq)

data Item = Item
  { itemType   :: ItemType
  , itemEffect :: String
  , itemValue  :: Int
  , itemPrice  :: Int
  } deriving (Show, Read, Eq)

data Battle
  = Fight
  | RunAway
  | VisitShop
  deriving (Show, Read, Enum, Bounded)

class Toys a where
  showItemToSell :: (Int, a) -> String
  showItemDetails :: a -> String

instance Toys Weapon where
  showItemToSell (i, weapon) = show i ++ ". " ++ showItemDetails weapon
  showItemDetails :: Weapon -> String
  showItemDetails Weapon {..} =
    let typeStr = show wType
        attackStr = show wAttack
        priceStr = show wPrice
        sellPriceStr = show wSellPrice
     in unlines
          [ "\nWeapon Type: " ++ typeStr
          , "Attack: " ++ attackStr
          , "Price: " ++ priceStr ++ " ada"
          , "Sell Price: " ++ sellPriceStr ++ " ada"
          ]

instance Toys Armor where
  showItemToSell (i, armor) = show i ++ ". " ++ showItemDetails armor
  showItemDetails Armor {..} =
    let typeStr = show aType
        defenseStr = show aDefense
        priceStr = show aPrice
        sellPriceStr = show aSellPrice
     in unlines
          [ "\nArmor Type: " ++ typeStr
          , "Defense: " ++ defenseStr
          , "Price: " ++ priceStr ++ " ada"
          , "Sell Price: " ++ sellPriceStr ++ " ada"
          ]

instance Toys Item where
  showItemToSell (i, item) = show i ++ ". " ++ showItemDetails item
  showItemDetails Item {..} =
    let typeStr = show itemType
        effectStr = itemEffect
        priceStr = show itemPrice
     in unlines
          [ "\nItem Type: " ++ typeStr
          , "Effect: " ++ effectStr
          , "Price: " ++ priceStr ++ " ada"
          ]

instance (Toys a, Toys b) => Toys (Either a b) where
  showItemToSell (i, Left item)  = showItemToSell (i, item)
  showItemToSell (i, Right item) = showItemToSell (i, item)
  showItemDetails (Left item)  = showItemDetails item
  showItemDetails (Right item) = showItemDetails item

golemAction :: IO ()
golemAction = do
  action <- randomRIO (1, 3) :: IO Int
  case action of
    1 -> putStrLn "The Golem attacks with a fire attack!"
    2 -> putStrLn "The Golem attacks with an ice attack!"
    3 -> putStrLn "The Golem attacks with an earth attack!"
    _ -> putStrLn "The Golem attacks with an earth attack!"

initializePlayer :: IO Player
initializePlayer =
  return
    Player
      { pAttack = 10
      , pHp = 100
      , pDefense = 5
      , pComboCount = 0
      , pWeapon = Nothing
      , pArmor = Nothing
      , pMoney = 50
      }

shopWeapons :: [Weapon]
shopWeapons = [Weapon Sword 10 10 5, Weapon Axe 15 15 8, Weapon Bow 12 12 6]

shopArmors :: [Armor]
shopArmors =
  [Armor Shield 10 10 5, Armor Helmet 8 8 4, Armor Chestplate 12 12 6]

shopItems :: [Item]
shopItems =
  [ Item HealingPotion "Restores 10 HP" 10 10
  , Item HealingPotion "Restores 8 HP" 8 8
  , Item HealingPotion "Restores 6 HP" 6 6
  , Item HealingPotion "Restores 20 HP" 20 20
  , Item Enhancement "Increases attack by 5" 5 5
  , Item Enhancement "Increases attack by 8" 8 8
  , Item Enhancement "Increases attack by 20" 20 20
  ]

loadFile :: Read a => FilePath -> IO (Maybe a)
loadFile file = do
  result <- try (readFile file) :: IO (Either SomeException String)
  return $ either (const Nothing) readMaybe result

saveFile :: FilePath -> Player -> IO ()
saveFile filePath contents = do
  (tempFile, tempHandle) <- openTempFile "." filePath
  hClose tempHandle
  writeFile tempFile (show contents)
  removeFile filePath
  renameFile tempFile filePath

battle :: IO Bool
battle = do
  putStrLn "\nYou've encountered a Golem!"
  gType <- fmap toEnum (randomRIO (0, fromEnum (maxBound :: GolemType)))
  gAttack <- randomRIO (15, 20)
  gHp <- randomRIO (40, 60)
  maybePlayer <- loadFile "player.txt" :: IO (Maybe Player)
  firstPlayer <- initializePlayer
  let golem = Golem {gType = gType, gAttack = gAttack, gHp = gHp}
      player = fromMaybe firstPlayer maybePlayer
  putStrLn $ "The Golem is of type: " ++ show gType
  displayUser golem player
  result <- optionResolve golem player
  return result

displayUser :: Golem -> Player -> IO ()
displayUser golem player = do
  if gHp golem <= 0 || pHp player <= 0
    then return ()
    else do
      putStrLn "--------------------------------------------------"
      putStrLn
        $ "The Golem has "
            ++ show (gHp golem)
            ++ " Health and "
            ++ show (gAttack golem)
            ++ " attack."
      putStrLn
        $ "You have "
            ++ show (pHp player)
            ++ " Health, "
            ++ show (pAttack player)
            ++ " attack"
            ++ fromMaybe
                 " ,0 Weapon ,"
                 (fmap
                    (\w -> " ,01 Weapon type : " ++ show (wType w))
                    (pWeapon player))
            ++ maybe
                 " ,0 Armor ,"
                 (\a -> " ,01 Armor type : " ++ show (aType a))
                 (pArmor player)
            ++ ", Total money : "
            ++ show (pMoney player)
            ++ " ada."
      putStrLn "Choose an action: \n1. Fight\n2. RunAway\n3. VisitShop?"
      putStrLn "--------------------------------------------------"

optionResolve :: Golem -> Player -> IO Bool
optionResolve golem player = do
  option <- getLine
  case option of
    "1" -> handleFight golem player
    "2" -> handleRunAway golem player
    "3" -> handleVisitShop golem player
    _ ->
      putStrLn
        "Choose an action: \n1. Fight\n2. RunAway\n3. VisitShop?\n--------------------------------------------------"
        >> optionResolve golem player

handleRunAway :: Golem -> Player -> IO Bool
handleRunAway golem player = do
  golemAction
  runAway golem player

handleFight :: Golem -> Player -> IO Bool
handleFight golem player = do
  golemAction
  let weaponBonus = maybe 0 wAttack (pWeapon player)
      attackBonus =
        if pComboCount player >= 2
          then 1.5 :: Double
          else 1.0 :: Double
      gDamage =
        round $ fromIntegral (pAttack player + weaponBonus) * attackBonus
      newGolem = golem {gHp = gHp golem - gDamage}
      golemDamageMultiplier =
        case gType golem of
          Fire  -> 1.2 :: Double
          Ice   -> 0.9 :: Double
          Earth -> 1.0 :: Double
      pDamage = round $ fromIntegral (gAttack golem) * golemDamageMultiplier
      armorDefense = maybe 0 aDefense (pArmor player)
      damageTaken = max (pDamage - armorDefense) 0
      newHp = pHp player - damageTaken
      newPlayer = player {pHp = newHp, pComboCount = pComboCount player + 1}
  displayUser newGolem newPlayer
  if newHp <= 0
    then putStrLn "You've lost the battle!" >> return False
    else if gHp newGolem <= 0
           then putStrLn "You've won the battle!" >> award newPlayer
           else optionResolve newGolem newPlayer

runAway :: Golem -> Player -> IO Bool
runAway golem player = do
  let golemDamageMultiplier =
        case gType golem of
          Fire  -> 1.2 :: Double
          Ice   -> 0.9 :: Double
          Earth -> 1.0 :: Double
      pDamage = round $ fromIntegral (gAttack golem) * golemDamageMultiplier
      armorDefense = maybe 0 aDefense (pArmor player)
      damageTaken = max (pDamage - armorDefense) 0
      newHp = pHp player - damageTaken
      newPlayer = player {pHp = newHp}
  displayUser golem newPlayer
  if newHp <= 0
    then putStrLn "You've lost the battle!" >> return False
    else optionResolve golem newPlayer

handleVisitShop :: Golem -> Player -> IO Bool
handleVisitShop golem player = do
  putStrLn "Welcome to the shop!"
  newPlayer <- shopAction player golem
  optionResolve golem newPlayer

shopAction :: Player -> Golem -> IO Player
shopAction player golem = do
  displayPlayer player
  putStrLn "--------------------------------------------------"
  putStrLn "Welcome to the shop! Choose an action:"
  putStrLn "1. Buy an item"
  putStrLn "2. Sell an item"
  putStrLn "3. Exit shop"
  putStrLn "--------------------------------------------------"
  action <- getLine
  case action of
    "1" -> purchaseItem player
    "2" -> sellItem player
    "3" -> optionResolve golem player >> return player
    _   -> shopAction player golem

purchaseItem :: Player -> IO Player
purchaseItem player = do
  putStrLn "--------------------------------------------------"
  putStrLn "Choose an item to buy:"
  putStrLn "1. Weapons"
  putStrLn "2. Armor"
  putStrLn "3. Potions"
  putStrLn "4. Enhancement Items"
  putStrLn "--------------------------------------------------"
  choice <- getLine
  case choice of
    "1" -> buyWeapon player
    "2" -> buyArmor player
    "3" -> buyPotion player
    "4" -> buyEnhancementItem player
    _   -> putStrLn "Invalid choice!" >> purchaseItem player

award :: Player -> IO Bool
award newPlayer = do
  putStrLn "Congratulations you have received your reward: 10 ada"
  let newPlayer' = newPlayer {pMoney = pMoney newPlayer + 10}
  putStrLn $ "Total ada: " ++ show (pMoney newPlayer')
  return True

--Buy Shop Action
buyWeapon :: Player -> IO Player
buyWeapon player = do
  putStrLn "you have weapon :"
  let availableWeapons = filter (\w -> pWeapon player /= Just w) shopWeapons
  putStrLn "Available weapons:"
  mapM_ (putStrLn . showItemToSell) (zip [1 ..] availableWeapons)
  putStrLn "Enter the number of the weapon you want to buy:"
  weaponChoice <- getLine
  let weaponIndex = readMaybe weaponChoice :: Maybe Int
  case weaponIndex of
    Just num ->
      case lookup num (zip [1 ..] availableWeapons) of
        Just weapon ->
          if pMoney player >= wPrice weapon
            then do
              let newMoney = pMoney player - wPrice weapon
                  newPlayer = player {pWeapon = Just weapon, pMoney = newMoney}
              putStrLn
                $ "You've bought: \n"
                    ++ showItemToSell (1, weapon)
                    ++ "\nTotal ada have left : "
                    ++ show newMoney
                    ++ " ada"
              return newPlayer
            else putStrLn "Not enough money!" >> return player
        _ -> putStrLn "Wrong choice" >> buyWeapon player
    Nothing -> putStrLn "Invalid choice!" >> buyWeapon player

buyArmor :: Player -> IO Player
buyArmor player = do
  let availableArmors = filter (\w -> pArmor player /= Just w) shopArmors
  putStrLn "Available armors:"
  mapM_ (putStrLn . showItemToSell) (zip [1 ..] availableArmors)
  putStrLn "Enter the number of the armor you want to buy:"
  armorChoice <- getLine
  let armorIndex = readMaybe armorChoice :: Maybe Int
  case armorIndex of
    Just num ->
      case lookup num (zip [1 ..] availableArmors) of
        Just armor ->
          if pMoney player >= aPrice armor
            then do
              let newMoney = pMoney player - aPrice armor
                  newPlayer = player {pArmor = Just armor, pMoney = newMoney}
              putStrLn
                $ "You've bought: "
                    ++ showItemToSell (1, armor)
                    ++ "\nTotal ada have left : "
                    ++ show newMoney
                    ++ " ada"
              return newPlayer
            else putStrLn "Not enough money!" >> return player
        _ -> putStrLn "Wrong choice!" >> buyArmor player
    Nothing -> putStrLn "Invalid choice!" >> buyArmor player

buyPotion :: Player -> IO Player
buyPotion player = do
  let availablePotions = filter ((== HealingPotion) . itemType) shopItems
  putStrLn "Available potions:"
  mapM_ (putStrLn . showItemToSell) (zip [1 ..] availablePotions)
  putStrLn "Enter the number of the potion you want to buy:"
  potionChoice <- getLine
  let potionIndex = readMaybe potionChoice :: Maybe Int
  case potionIndex of
    Just num ->
      case lookup num (zip [1 ..] availablePotions) of
        Just potion ->
          if pMoney player >= itemPrice potion
            then do
              let newMoney = pMoney player - itemPrice potion
                  newHp = pHp player + itemValue potion
                  newPlayer = player {pMoney = newMoney, pHp = newHp}
              putStrLn "--------------------------------------------------"
              putStrLn
                $ "You've bought: "
                    ++ showItemToSell (1, potion)
                    ++ "\nTotal ada have left : "
                    ++ show newMoney
                    ++ " ada"
              displayPlayer newPlayer
              return newPlayer
            else putStrLn "Not enough money!" >> return player
        _ -> putStrLn "Wrong choice!" >> buyPotion player
    Nothing -> putStrLn "Invalid choice!" >> buyPotion player

buyEnhancementItem :: Player -> IO Player
buyEnhancementItem player = do
  let availableItems = filter ((== Enhancement) . itemType) shopItems
  putStrLn "Available enhancement items:"
  mapM_ (putStrLn . showItemToSell) (zip [1 ..] availableItems)
  putStrLn "Enter the number of the enhancement item you want to buy:"
  itemChoice <- getLine
  let itemIndex = readMaybe itemChoice :: Maybe Int
  case itemIndex of
    Just num ->
      case lookup num (zip [1 ..] availableItems) of
        Just item ->
          if pMoney player >= itemPrice item
            then do
              let newMoney = pMoney player - itemPrice item
                  newPattack = pAttack player + itemValue item
                  newPlayer = player {pMoney = newMoney, pAttack = newPattack}
              putStrLn "--------------------------------------------------"
              putStrLn
                $ "You've bought: "
                    ++ showItemToSell (1, item)
                    ++ "\nTotal ada have left : "
                    ++ show newMoney
                    ++ " ada"
              displayPlayer newPlayer
              return newPlayer
            else putStrLn "Not enough money!" >> return player
        _ -> putStrLn "Wrong choice!" >> buyEnhancementItem player
    Nothing -> putStrLn "Invalid choice!" >> buyEnhancementItem player

-- Sell items
sellItem :: Player -> IO Player
sellItem player = do
  let itemsForSale =
        catMaybes [fmap Left (pWeapon player), fmap Right (pArmor player)]
  putStrLn "--------------------------------------------------"
  putStrLn "You can sell the following items:"
  mapM_ (putStrLn . showItemToSell) (zip [1 ..] itemsForSale)
  putStrLn "Enter the number of the item you want to sell:"
  choice <- getLine
  case readMaybe @Int choice of
    Just i -> do
      let itemToSell =
            fromMaybe (head itemsForSale) (lookup i (zip [1 ..] itemsForSale))
          newPlayer =
            case itemToSell of
              Left weapon ->
                player
                  { pMoney = pMoney player + wSellPrice weapon
                  , pWeapon = Nothing
                  }
              Right armor ->
                player
                  {pMoney = pMoney player + aSellPrice armor, pArmor = Nothing}
      putStrLn
        $ "You've sold: "
            ++ showItemToSell (1, itemToSell)
            ++ "\nTotal ada have left : "
            ++ show (pMoney newPlayer)
            ++ " ada"
      return newPlayer
    Nothing -> putStrLn "Invalid choice!" >> sellItem player

displayPlayer :: Player -> IO ()
displayPlayer player =
  let basicStats =
        "\nPlayer Status :\n"
          ++ "Attack: "
          ++ show (pAttack player)
          ++ "\n"
          ++ "Health: "
          ++ show (pHp player)
          ++ "\n"
          ++ "Defense: "
          ++ show (pDefense player)
          ++ "\n"
          ++ "Combo Count: "
          ++ show (pComboCount player)
          ++ "\n"
      weaponInfo =
        case pWeapon player of
          Just weapon -> "\nWeapon: \n" ++ showWeapon' weapon ++ "\n"
          Nothing     -> "No weapon equipped.\n"
      armorInfo =
        case pArmor player of
          Just armor -> "\nArmor: \n" ++ shopArmors' armor ++ "\n"
          Nothing    -> "No armor equipped.\n"
   in putStrLn
        $ "--------------------------------------------------------------"
            ++ basicStats
            ++ weaponInfo
            ++ armorInfo
            ++ "\nMoney: "
            ++ show (pMoney player)
            ++ " ada"
            ++ "\n--------------------------------------------------------------------"
            ++ "\n"

showWeapon' :: Weapon -> String
showWeapon' Weapon {..} =
  "Type : "
    ++ show wType
    ++ ", Attack : "
    ++ show wAttack
    ++ ", Buy Price : "
    ++ show wPrice
    ++ " ada "
    ++ ", Sell Price : "
    ++ show wSellPrice
    ++ " ada "

shopArmors' :: Armor -> String
shopArmors' Armor {..} =
  "Type : "
    ++ show aType
    ++ ", Defense "
    ++ show aDefense
    ++ ", Buy Price : "
    ++ show aPrice
    ++ " ada "
    ++ ", Sell Price : "
    ++ show aSellPrice
    ++ " ada "
