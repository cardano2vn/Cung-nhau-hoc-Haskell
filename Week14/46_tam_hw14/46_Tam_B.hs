{-# LANGUAGE ScopedTypeVariables #-}

module Homework15B where

import Control.Exception (try)

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- IMPORTANT: Read the README.md file before completing the homework.
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- 1. Write a function that takes a list and returns the head if the list is not empty.
-- If the list is empty, return Nothing.

headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe (x:_) = Just x

-- 2. Write a function that takes a list of Maybe values and returns a list of all the Just values.
-- If there are no Just values, return an empty list.

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Nothing:xs) = catMaybes xs
catMaybes (Just x:xs) = x : catMaybes xs

-- 3. Write a function that tries to read from a file and returns the contents of the file.
-- If the file does not exist, return Nothing.

readFileMaybe :: FilePath -> IO (Maybe String)
readFileMaybe filename = do 
    result <- try $ readFile filename
    case result of 
      Left (e :: IOError) -> return Nothing
      Right content -> return $ Just content

-- 4. Write a function that checks all the requirements for a password using the
-- Either type with a custom data type for errors.
-- The requirements are:
-- - The password must be at least 10 characters long.
-- - The password must contain at least one digit.
-- - The password must contain at least one uppercase letter.
-- - The password must contain at least one lowercase letter.

data PasswordError = NotLongEnough | NoDigit | NoUpperChar | NoLowerChar deriving (Show)

passwordLongEnough :: String -> Either PasswordError String
passwordLongEnough s | length s >= 10 = Right s
passwordLongEnough s = Left NotLongEnough

passwordHasDigit :: String -> Either PasswordError String
passwordHasDigit s | any (`elem` ['0'..'9']) s = Right s
passwordHasDigit _ = Left NoDigit

passwordHasUppercase :: String -> Either PasswordError String
passwordHasUppercase s | any (`elem` ['A'..'Z']) s = Right s
passwordHasUppercase _ = Left NoUpperChar

passwordHasLowercase :: String -> Either PasswordError String
passwordHasLowercase s | any (`elem` ['a'..'z']) s = Right s
passwordHasLowercase _ = Left NoLowerChar

-- short-circuit with Either in do notation
passwordRequirements :: String -> Either PasswordError String
passwordRequirements s = do
    s1 <- passwordLongEnough s
    s2 <- passwordHasDigit s1
    s3 <- passwordHasUppercase s2
    passwordHasLowercase s3