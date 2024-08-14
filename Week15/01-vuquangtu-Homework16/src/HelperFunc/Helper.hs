module HelperFunc.Helper where

import           User.Types

import           Data.Maybe (fromMaybe, isNothing)

prompt :: String -> IO String
prompt text = putStrLn text >> getLine

makeEmtyBoard :: Moves -> Board
makeEmtyBoard moves = replicate (maxR + 5) (replicate (maxC + 5) Nothing)
  where
    (maxR, maxC) = getMaxRC moves

getMaxRC :: Moves -> Location
getMaxRC moves = (maxR, maxC)
  where
    locations = map snd moves
    maxR = maximum (map fst locations)
    maxC = maximum (map snd locations)

playGame :: Moves -> Board
playGame moves =
  case moves of
    [] -> replicate 5 (replicate 5 Nothing)
    _ ->
      foldr
        (\(player, (r, c)) acc -> fromMaybe acc (makeMove acc (r, c) player))
        emptyBoard
        moves
  where
    emptyBoard = makeEmtyBoard moves

showBoard :: Moves -> String
showBoard moves = unlines $ columnIndices : rowIndices
  where
    columnIndices = "  " ++ unwords (map show ([1 .. 9] :: [Integer]))
    rowIndices =
      zipWith
        (\i row -> unwords (map showCell row) ++ "  " ++ show (i :: Integer))
        [0 ..]
        (playGame moves)
    showCell Nothing  = "."
    showCell (Just X) = "X"
    showCell (Just O) = "O"

makeMove :: Board -> Location -> Player -> Maybe Board
makeMove board (r, c) player
  | isNothing (board !! r !! c) =
    Just
      (take r board
         ++ [take c (board !! r) ++ [Just player] ++ drop (c + 1) (board !! r)]
         ++ drop (r + 1) board)
  | otherwise = Nothing
