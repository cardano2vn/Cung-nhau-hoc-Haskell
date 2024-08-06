{-# LANGUAGE ScopedTypeVariables #-}

module Homework15B where

import System.IO
import Control.Exception --(IOException)
import Data.Char

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- IMPORTANT: Read the README.md file before completing the homework.
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- 1. Write a function that takes a list and returns the head if the list is not empty.
-- If the list is empty, return Nothing.

headMaybe' :: [a] -> Maybe a
-- headMaybe = undefined
headMaybe' (x:xs) = Just x
headMaybe' _ = Nothing
{- ex:
      headMaybe' []
      headMaybe' [1, 2, 3]
-}

-- 2. Write a function that takes a list of Maybe values and returns a list of all the Just values.
-- If there are no Just values, return an empty list.

catMaybes' :: [Maybe a] -> [a]
-- -- catMaybes = undefined
catMaybes' [] = []
catMaybes' (Nothing:xs) = catMaybes' xs
catMaybes' (Just x:xs) = x : catMaybes' xs
{- ex: 
      catMaybes' [Just 1, Nothing, Just 2, Just 3]
      catMaybes' [Just 1, Just 2, Just 3, Just 4]
      catMaybes' [Nothing, Nothing, Nothing]
-}


-- 3. Write a function that tries to read from a file and returns the contents of the file.
-- If the file does not exist, return Nothing.

readFileMaybe' :: FilePath -> IO (Maybe String)
-- readFileMaybe = undefined
readFileMaybe' path = do      
      ec <- try $ readFile path
      case ec of
            Left (e :: IOException) -> return Nothing
            Right c -> return (Just c)            


-- 4. Write a function that checks all the requirements for a password using the
-- Either type with a custom data type for errors.
-- The requirements are:
-- - The password must be at least 10 characters long.
-- - The password must contain at least one digit.
-- - The password must contain at least one uppercase letter.
-- - The password must contain at least one lowercase letter.

data PasswordError = WrongConstructor deriving (Show, Eq)

passwordLongEnough :: String -> Either PasswordError String
passwordLongEnough pw
      | (length pw) < 10 = Left WrongConstructor
      | (length pw) >= 10 = Right "The Password is more than 10 characters long"

passwordHasDigit :: String -> Either PasswordError String
passwordHasDigit pw
      | (any (isDigit) pw) == False = Left WrongConstructor
      | (any (isDigit) pw) == True = Right "The Password has at least one digit"

passwordHasUppercase :: String -> Either PasswordError String
passwordHasUppercase pw 
      | (any (isUpper) pw) == False = Left WrongConstructor
      | (any (isUpper) pw) == True = Right "The Password has at least one uppercase letter"

passwordHasLowercase :: String -> Either PasswordError String
passwordHasLowercase pw
      | (any (isLower) pw) == False = Left WrongConstructor
      | (any (isLower) pw) == True = Right "The Password has at least one lowercase letter"

passwordRequirements :: String -> Either PasswordError String
passwordRequirements = undefined
      


