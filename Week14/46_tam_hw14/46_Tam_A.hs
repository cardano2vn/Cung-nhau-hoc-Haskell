module Main where
import Text.Read (readMaybe)
import Control.Exception (try)

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


-- Bug1: read -> readMayBe in "+" command, when parsing number input
-- Bug2: not catching error writeFile
-- Bug3: not catching error readFile
-- Bug4: read -> readMayBe when parsing file content

printTodoItem :: (Int, String) -> IO ()
printTodoItem (n, todo) = putStrLn (show n ++ ": " ++ todo)

prompt :: [String] -> IO ()
prompt todos = do
  putStrLn ""
  putStrLn "Current TODO list:"
  foldr (\x k -> printTodoItem x >> k) (return ()) (zip [0 ..] todos)
  command <- getLine
  interpretCommand command todos

delete :: Maybe Int -> [a] -> [a]
delete _ [] = []
delete Nothing as = as
delete (Just 0) (_ : as) = as
delete (Just n) (a : as) = a : delete (Just(n - 1)) as

interpretCommand :: String -> [String] -> IO ()
interpretCommand cmd todos = case cmd of
  "q" -> return ()
  ('+' : ' ' : todo) -> prompt (todo : todos)
  -- Bug1
  ('-' : ' ' : num) -> prompt $ delete (readMaybe num) todos
  ('s' : ' ' : filename) -> do
    -- Bug 2
    result  <- try $ writeFile filename (show todos) 
    case result of 
      Left (e :: IOError) -> putStrLn $ "Could not write file, error: " ++ show e
      Right _ -> putStrLn "File is written."
  ('l' : ' ' : filename) -> do 
    -- Bug 3
    result <- try $ readFile filename
    case result of 
      Left (e :: IOError) -> putStrLn $ "Could not read file, error: " ++ show e
      Right content ->
      -- Bug 4
        case readMaybe content of
          Nothing ->  putStrLn "Could not parse file content"
          Just todos' -> prompt todos'
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