{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Text.Read (readMaybe)
import System.IO ()
import Control.Exception ( IOException, try, catch )

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- IMPORTANT: Read the README.md file before completing the homework.
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- This is a CLI application that allows the user to manage a TODO list.
-- The user can add and remove items from the list and save and load the list
-- from a file.
-- It's a working prototype, but it has some bugs. Specifically, it doesn't
-- handle errors very well. Your mission, should you choose to accept it, is to
-- fix those bugs and make the application more robust. Hint: Try to interact
-- with the application in unexpected ways and see what happens! You should be
-- able to find and fix 3 bugs.

printTodoItem :: (Int, String) -> IO ()
printTodoItem (n, todo) = putStrLn (show n ++ ": " ++ todo)

prompt :: [String] -> IO ()
prompt todos = do
  putStrLn ""
  putStrLn "Current TODO list:"
  foldr (\x k -> printTodoItem x >> k) (return ()) (zip [0 ..] todos)
  command <- getLine
  interpretCommand command todos

-- delete :: Int -> [a] -> [a]
-- delete 0 (_ : as) = as
-- delete _ [] = []
-- delete n (a : as) = a : delete (n - 1) as

delete :: Maybe Int -> [a] -> [a]
delete Nothing (a:as) = a : as
delete (Just 0) (_ : as) = as
delete _ [] = []
delete (Just n) (a : as) = a : delete (Just (n - 1)) as

interpretCommand :: String -> [String] -> IO ()
interpretCommand cmd todos = case cmd of
  "q" -> return ()
  ('+' : ' ' : todo) -> prompt (todo : todos)
  ('-' : ' ' : num) -> prompt $ delete (readMaybe num) todos -- bug: (-) String \n fix by readMaybe or readEither
  ('s' : ' ' : fn) -> do
                      catch (writeFile fn (show todos)) handler -- bug: s " * : / \ |< > ? -> fix by catch 
                      where 
                        handler :: IOException -> IO ()
                        handler e = do
                          putStrLn ("Warning : invalid name, maybe it includes the characters \" * : / \ \| < > ? \n" ++ show e) >> printCommands
  ('l' : ' ' : fn) -> do -- readFile fn >>= prompt . read -- bug: file does not exist \n fix by try - IOException
                      ec <- try $ readFile fn
                      case ec of
                        Left (e :: IOException) -> putStrLn ("Warning: the file does not exit in the current directory \n" ++ show e) >> main
                        Right c -> prompt $ read c
  _ -> do
      putStrLn ("Invalid command: `" ++ cmd ++ "`")
      prompt todos

printCommands :: IO ()
printCommands = do
  putStrLn "Commands:"
  putStrLn "+ <Item Name>   - Add a TODO entry"
  putStrLn "- <Item Number> - Delete the numbered entry"
  putStrLn "s <File Name>   - Save the current list of TODOs"
  putStrLn "l <File Name>   - Load the saved list of TODOs"
  putStrLn "q               - Quit without saving"

main :: IO ()
main = do
  printCommands
  prompt []