module Utils.Helpers where

import           Control.Monad       (when)
import           Data.Char
import           Data.List
import           Data.List.Split
import           System.Console.ANSI
import           System.IO
import           System.Random
import           Text.Printf         (printf)

data GameState
  = Exit
  | Finish

getCharAndCheckEsc :: IO (Maybe Char)
getCharAndCheckEsc = do
  c <- getChar
  case c of
    '\ESC' -> return Nothing
    _      -> return $ Just c

boldText :: String -> String
boldText text = "\ESC[1m" ++ text ++ "\ESC[0m"