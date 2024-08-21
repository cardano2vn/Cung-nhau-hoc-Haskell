module Main where

import           Games.Game2048
import           Games.Hangman
import           System.Console.ANSI
import           System.IO
import           Utils.Helpers

data MenuItem = MenuItem
  { menuItemText   :: String
  , menuItemAction :: IO ()
  }

mainMenuItems :: [MenuItem]
mainMenuItems =
  [ MenuItem "Hangman" (runGame hangman)
  , MenuItem "2048" (runGame game2048)
  , MenuItem "Quit" (putStrLn "Thanks for playing! Goodbye." >> return ())
  ]

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stdin NoBuffering
  hideCursor
  mainMenu 0

mainMenu :: Int -> IO ()
mainMenu selected = do
  clearScreen
  setCursorPosition 0 0
  putStrLn "Welcome to the Game Center!"
  putStrLn "Use 'w' to move up, 's' to move down, and Enter to select.\n"
  mapM_ (uncurry (printMenuItem selected)) (zip [0 ..] mainMenuItems)
  key <- getChar
  case key of
    'w'  -> mainMenu (max 0 (selected - 1))
    's'  -> mainMenu (min (length mainMenuItems - 1) (selected + 1))
    '\n' -> menuItemAction (mainMenuItems !! selected)
    _    -> mainMenu selected

printMenuItem :: Int -> Int -> MenuItem -> IO ()
printMenuItem selected index (MenuItem text _) = do
  if selected == index
    then do
      setSGR [SetColor Background Vivid Blue]
      putStr "> "
    else putStr "  "
  putStr text
  setSGR [Reset]
  putStrLn ""

runGame :: IO GameState -> IO ()
runGame gameAction = do
  gameState <- gameAction
  case gameState of
    Exit   -> mainMenu 0
    Finish -> askPlayAgain gameAction

askPlayAgain :: IO GameState -> IO ()
askPlayAgain gameAction = do
  putStr "Do you want to play again? (y/n): "
  response <- getChar
  case response of
    'y' -> runGame gameAction
    'n' -> mainMenu 0
    _ -> do
      putStrLn "\nInvalid input. Please enter 'y' or 'n'."
      askPlayAgain gameAction
