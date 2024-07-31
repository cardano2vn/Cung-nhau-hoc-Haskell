module Forest.Level1
  ( Forest(..)
  , level1forest
  ) where

data Forest a
  = FoundExit
  | Trail a (Forest a) (Forest a) (Forest a)
  deriving (Show)

level1forest :: (Num a, Ord a) => Forest a
level1forest =
  Trail
    3_000
    (Trail
       7_000
       (Trail 3_000 FoundExit FoundExit FoundExit)
       (Trail 4_000 FoundExit FoundExit FoundExit)
       (Trail 5_000 FoundExit FoundExit FoundExit))
    (Trail
       3000
       (Trail 3000 FoundExit FoundExit FoundExit)
       (Trail 9000 FoundExit FoundExit FoundExit)
       (Trail 5000 FoundExit FoundExit FoundExit))
    (Trail
       5000
       (Trail 3000 FoundExit FoundExit FoundExit)
       (Trail 4000 FoundExit FoundExit FoundExit)
       (Trail 1000 FoundExit FoundExit FoundExit))
