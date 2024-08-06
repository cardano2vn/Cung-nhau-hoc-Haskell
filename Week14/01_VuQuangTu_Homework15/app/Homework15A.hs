module Main where

import           Control.Exception (IOException, catch, try)
import           Data.List         (find)
import           System.Directory  (listDirectory)
import           Text.Read         (readMaybe)

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

delete :: Int -> [a] -> [a]
delete 0 (_:as) = as
delete _ []     = []
delete n (a:as) = a : delete (n - 1) as

interpretCommand :: String -> [String] -> IO ()
interpretCommand cmd todos =
  case cmd of
    "q" -> return ()
    ('+':' ':todo) -> prompt (todo : todos)
    ('-':' ':numStr) -> do
      case readMaybe numStr :: Maybe Int of
        Nothing ->
          putStrLn "Invalid numbered entry, please try again'" >> prompt todos
        Just num -> do
          if num < 0 || num > length todos - 1
            then putStrLn "Out of numbered entry, please try again"
                   >> prompt todos
            else do
              putStrLn
                $ "Deleted :" ++ show num ++ " numbered entry Successfull!"
              prompt $ delete num todos
    ('s':' ':fn) -> do
      result <- saveFileSafe fn (show todos)
      case result of
        Left e -> do
          putStrLn $ "Error saving : " ++ e
          prompt todos
        Right _ -> putStrLn $ "Save todo list successfull to file :" ++ fn
    ('l':' ':fn) -> do
      files <- listDirectory "."
      case find (== fn) files of
        Nothing -> putStrLn "no file exits, please try again" >> prompt todos
        Just fileName -> do
          result <- loadSafeFile fileName
          case result of
            Left e         -> putStrLn $ "Error to load file : " ++ e
            Right contents -> prompt (read contents :: [String])
    _ -> do
      putStrLn ("Invalid command: `" ++ cmd ++ "`")
      prompt todos

loadSafeFile :: FilePath -> IO (Either String String)
loadSafeFile fileName =
  catch (fmap Right . readFile $ fileName) handleIOexception
  where
    handleIOexception :: IOException -> IO (Either String String)
    handleIOexception e = return $ Left . show $ e

saveFileSafe :: FilePath -> String -> IO (Either String ())
saveFileSafe file contents = do
  result <- try (writeFile file contents) :: IO (Either IOException ())
  case result of
    Left e  -> return $ Left . show $ e
    Right _ -> return $ Right ()

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
