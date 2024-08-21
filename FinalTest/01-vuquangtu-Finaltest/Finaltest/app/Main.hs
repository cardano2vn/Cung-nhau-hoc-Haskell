module Main where

import           SelectMenu.SelectMenu
import           System.Console.ANSI   (Color (..), ColorIntensity (..),
                                        ConsoleLayer (..), SGR (..),
                                        clearScreen, setCursorPosition)
import           Utility.Color

main :: IO ()
main = do
  clearScreen
  setCursorPosition 0 0
  putStrLn
    $ setSGRCode [SetColor Foreground Vivid Yellow]
        ++ "Well Come to Mini Haskell Games"
        ++ setSGRCode [Reset]
  putStrLn "\nPress 'w','s' to move up ,down, ESC to exit."
  result <- mainSelect
  case result of
    "Exit" -> return ()
    _      -> main
