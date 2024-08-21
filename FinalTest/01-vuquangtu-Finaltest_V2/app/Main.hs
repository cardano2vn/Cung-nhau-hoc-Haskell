module Main where

import           SelectMenu.SelectMenu

import           System.Console.ANSI

import           Utility.Color

main :: IO ()
main = do
  clearScreen
  setCursorPosition 0 0
  -- putStrLn
  --   $ setSGRCode
  --       [SetColor Foreground Vivid Red, SetColor Background Vivid Green]
  --       ++ "Wellcome to Mini Haskell Games"
  --       ++ setSGRCode [Reset]
  putStrLn
    $ applyEffects
        [redText, greenBackground, underlineText, boldText, italicText]
        "Wellcome to Mini Haskell Games2"
  result <- mainSelect
  case result of
    "Exit" -> return ()
    _      -> main
