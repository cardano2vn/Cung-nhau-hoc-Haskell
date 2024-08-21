module Utility.Color where

import           Data.List           (intercalate)
import           System.Console.ANSI

setSGRCode :: [SGR] -> String
setSGRCode sgrs = "\ESC[" ++ intercalate ";" (map sgrCode sgrs) ++ "m"
  where
    sgrCode Reset                               = "0"
    sgrCode (SetColor Foreground Vivid Black)   = "90"
    sgrCode (SetColor Foreground Vivid Red)     = "91"
    sgrCode (SetColor Foreground Vivid Green)   = "92"
    sgrCode (SetColor Foreground Vivid Yellow)  = "93"
    sgrCode (SetColor Foreground Vivid Blue)    = "94"
    sgrCode (SetColor Foreground Vivid Magenta) = "95"
    sgrCode (SetColor Foreground Vivid Cyan)    = "96"
    sgrCode (SetColor Foreground Vivid White)   = "97"
    sgrCode (SetColor Background Vivid Black)   = "100"
    sgrCode (SetColor Background Vivid Red)     = "101"
    sgrCode (SetColor Background Vivid Green)   = "102"
    sgrCode (SetColor Background Vivid Yellow)  = "103"
    sgrCode (SetColor Background Vivid Blue)    = "104"
    sgrCode (SetColor Background Vivid Magenta) = "105"
    sgrCode (SetColor Background Vivid Cyan)    = "106"
    sgrCode (SetColor Background Vivid White)   = "107"
    sgrCode _                                   = "0"
