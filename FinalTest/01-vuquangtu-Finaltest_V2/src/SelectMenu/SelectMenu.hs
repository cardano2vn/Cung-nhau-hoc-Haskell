module SelectMenu.SelectMenu where

import           Game2048.Game2048   (mainGame2048)
import           HangMan.HangMan     (mainHangMan)

import           System.Console.ANSI
import           System.IO           (BufferMode (NoBuffering), hSetBuffering,
                                      hSetEcho, stdin)
import           Utility.Color

menuItems :: [String]
menuItems = ["HangMan", "2048", "Quit"]

displayMenu :: [String] -> Int -> IO ()
displayMenu items selectedIndex = do
  setCursorPosition 2 0
  clearFromCursorToScreenEnd
  mapM_ (putStrLn . formatItem) (zip [0 ..] items)
  where
    formatItem (i, item) =
      if i == selectedIndex
        then do
               setSGRCode [SetColor Foreground Vivid Green]
               ++ (">> " ++ item ++ " <<")
               ++ setSGRCode [Reset]
        else "   " ++ item

menuLoop :: [String] -> Int -> IO String
menuLoop items selectedIndex = do
  displayMenu items selectedIndex
  key <- getChar
  let itemCount = length items
  case key of
    '\ESC' -> do
      putStrLn "Exiting the program..."
      return "Exit"
    '\n' ->
      case items !! selectedIndex of
        "HangMan" -> mainHangMan >> return "HangManQuit"
        "2048"    -> mainGame2048 >> return "2048Quit"
        _         -> return "Exit"
    'w' -> do
      let newIndex = (selectedIndex - 1) `mod` itemCount
      menuLoop items newIndex
    's' -> do
      let newIndex = (selectedIndex + 1) `mod` itemCount
      menuLoop items newIndex
    _ -> menuLoop items selectedIndex

mainSelect :: IO String
mainSelect = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  menuLoop menuItems 0
