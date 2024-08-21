module Utility.Color where

import           System.Console.ANSI

colorText :: Color -> String -> String
colorText color text =
  setSGRCode [SetColor Foreground Vivid color] ++ text ++ setSGRCode [Reset]

blackText, redText, greenText, yellowText, blueText, magentaText, cyanText, whiteText ::
     String -> String
blackText = colorText Black

redText = colorText Red

greenText = colorText Green

yellowText = colorText Yellow

blueText = colorText Blue

magentaText = colorText Magenta

cyanText = colorText Cyan

whiteText = colorText White




----------------------------------------------
backgroundText :: Color -> String -> String
backgroundText color text =
  setSGRCode [SetColor Background Vivid color] ++ text ++ setSGRCode [Reset]

blackBackground, redBackground, greenBackground, yellowBackground, blueBackground, magentaBackground, cyanBackground, whiteBackground ::
     String -> String
blackBackground = backgroundText Black

redBackground = backgroundText Red

greenBackground = backgroundText Green

yellowBackground = backgroundText Yellow

blueBackground = backgroundText Blue

magentaBackground = backgroundText Magenta

cyanBackground = backgroundText Cyan

whiteBackground = backgroundText White

----------------------------------------------
coloredText :: Color -> Color -> String -> String
coloredText fg bg text =
  setSGRCode [SetColor Foreground Vivid fg, SetColor Background Vivid bg]
    ++ text
    ++ setSGRCode [Reset]

redOnBlack, greenOnWhite, yellowOnWhite, redOnWhite :: String -> String
redOnBlack = coloredText Red Black

greenOnWhite = coloredText Green White

yellowOnWhite = coloredText Yellow White

redOnWhite = coloredText Red White

----------------------------------------------
boldText :: String -> String
boldText text =
  setSGRCode [SetConsoleIntensity BoldIntensity] ++ text ++ setSGRCode [Reset]

italicText :: String -> String
italicText text = setSGRCode [SetItalicized True] ++ text ++ setSGRCode [Reset]

underlineText :: String -> String
underlineText text =
  setSGRCode [SetUnderlining SingleUnderline] ++ text ++ setSGRCode [Reset]

applyEffects :: [String -> String] -> String -> String
applyEffects listColorFun text = foldr (\f acc -> f acc) text listColorFun

----------------------------------------------------------------------------------------------
-- Xác định màu sắc cho các giá trị
colorForValue :: Int -> [SGR]
colorForValue 0 = [Reset]
colorForValue 2 = [SetColor Foreground Vivid Yellow]
colorForValue 4 = [SetColor Foreground Vivid Green]
colorForValue 8 = [SetColor Foreground Vivid Cyan]
colorForValue 16 = [SetColor Foreground Vivid Blue]
colorForValue 32 = [SetColor Foreground Vivid Red]
colorForValue 64 = [SetColor Foreground Vivid Magenta]
colorForValue 128 = [SetColor Foreground Vivid White]
colorForValue 256 =
  [SetColor Foreground Vivid Yellow, SetColor Background Vivid Blue]
colorForValue 512 =
  [SetColor Foreground Vivid Cyan, SetColor Background Vivid Green]
colorForValue 1024 =
  [SetColor Foreground Vivid White, SetColor Background Vivid Red]
colorForValue 2048 =
  [SetColor Foreground Vivid Black, SetColor Background Vivid White]
colorForValue _ = [Reset]
