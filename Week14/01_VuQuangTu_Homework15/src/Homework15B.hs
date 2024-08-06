{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-max-refinement-hole-fits #-}
{-# LANGUAGE LambdaCase          #-}

module Homework15B where

import           Data.Char        (isDigit, isLower, isUpper)
import           Data.Either      (lefts)
import           System.Directory

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- IMPORTANT: Read the README.md file before completing the homework.
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- 1. Write a function that takes a list and returns the head if the list is not empty.
-- If the list is empty, return Nothing.
headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe xs = Just . head $ xs

-- 2. Write a function that takes a list of Maybe values and returns a list of all the Just values.
-- If there are no Just values, return an empty list.
--cach1
-- catMaybes :: [Maybe a] -> [a]
-- catMaybes [] = []
-- catMaybes xs = concatMap getJustValue xs
-- getJustValue :: Maybe a -> [a]
-- getJustValue Nothing  = []
-- getJustValue (Just x) = [x]
--cach 2
catMaybes :: [Maybe a] -> [a]
catMaybes =
  concatMap
    (\case
       Nothing -> []
       Just x -> [x])

-- 3. Write a function that tries to read from a file and returns the contents of the file.
-- If the file does not exist, return Nothing.
readFileMaybe :: FilePath -> IO (Maybe String)
readFileMaybe file = do
  paths <- listDirectory "."
  if file `notElem` paths
    then return Nothing
    else do
      contents <- readFile file
      return $ Just contents

-- 4. Write a function that checks all the requirements for a password using the
-- Either type with a custom data type for errors.
-- The requirements are:
-- - The password must be at least 10 characters long.
-- - The password must contain at least one digit.
-- - The password must contain at least one uppercase letter.
-- - The password must contain at least one lowercase letter.
data PasswordError =
  WrongConstructor
  deriving (Show)

passwordLongEnough :: String -> Either PasswordError String
passwordLongEnough password
  | length password <= 10 = Left WrongConstructor
  | otherwise = Right password

passwordHasDigit :: String -> Either PasswordError String
passwordHasDigit password
  | any isDigit password = Right password
  | otherwise = Left WrongConstructor

passwordHasUppercase :: String -> Either PasswordError String
passwordHasUppercase password
  | any isUpper password = Right password
  | otherwise = Left WrongConstructor

passwordHasLowercase :: String -> Either PasswordError String
passwordHasLowercase password
  | any isLower password = Right password
  | otherwise = Left WrongConstructor

passwordRequirements :: String -> Either PasswordError String
passwordRequirements password
  | any (\l -> l `elem` ['@', '!', '.']) password = Right password
  | otherwise = Left WrongConstructor
--phát triển thêm validate tổng password
-- passwordLongEnough :: String -> Either String String
-- passwordLongEnough password
--   | length password <= 10 = Left "Not Long Enough"
--   | otherwise = Right password
-- passwordHasDigit :: String -> Either String String
-- passwordHasDigit password
--   | any isDigit password = Right password
--   | otherwise = Left "Not has digit"
-- --   | any (\l -> l `elem` ['0' .. '9']) password = Right password
-- passwordHasUppercase :: String -> Either String String
-- passwordHasUppercase password
--   -- | any (\l -> l `elem` ['A' .. 'Z']) password = Right password
--   | any isUpper password = Right password
--   | otherwise = Left "not has uppercase"
-- passwordHasLowercase :: String -> Either String String
-- passwordHasLowercase password
--   | any isLower password = Right password
--   | otherwise = Left "not has lowercase"
-- passwordRequirements :: String -> Either String String
-- passwordRequirements password
--   | any (\l -> l `elem` ['@', '!', '.']) password = Right password
--   | otherwise = Left "not has reqirements"
-- passwordVadilate :: String -> IO ()
-- passwordVadilate password =
--   case result of
--     [] -> putStrLn $ "Password : " ++ password
--     _  -> mapM_ putStrLn result
--   where
--     result =
--       lefts
--         [ passwordLongEnough password
--         , passwordHasDigit password
--         , passwordHasUppercase password
--         , passwordHasLowercase password
--         , passwordRequirements password
--         ]
