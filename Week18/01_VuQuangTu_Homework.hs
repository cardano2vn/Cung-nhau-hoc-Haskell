{-# LANGUAGE InstanceSigs #-}

import           Data.Functor
import           Data.Semigroup (Sum (..))

----------------------------------------------------------------------------------------------------
---------------------------------------- QUESTION 1 ------------------------------------------------
{-
Question 1: Implement the Functor instance for a RoseTree type.
A RoseTree is a tree where each node can have any number of children.
It's a bit more complicated than the binary tree we saw in the lecture,
but here's the trick to solve it: Follow the types!
-}
data RoseTree a =
  RoseNode a [RoseTree a]
  deriving (Eq, Show)

exampleRoseTree :: RoseTree Int
exampleRoseTree = RoseNode 1 [RoseNode 2 [], RoseNode 3 [RoseNode 4 []]]

instance Functor RoseTree where
  fmap :: (a -> b) -> RoseTree a -> RoseTree b
  fmap f (RoseNode x children) = RoseNode (f x) (map (fmap f) children)

-- TODO: Implement the Functor instance for RoseTree
-- Test it out:
--- >>> fmap (+1) exampleRoseTree
-- RoseNode 2 [RoseNode 3 [],RoseNode 4 [RoseNode 5 []]]
--- >>> (*2) <$> exampleRoseTree
-- RoseNode 2 [RoseNode 4 [],RoseNode 6 [RoseNode 8 []]]
--- >>> exampleRoseTree $> "Yes!"
-- RoseNode "Yes!" [RoseNode "Yes!" [],RoseNode "Yes!" [RoseNode "Yes!" []]]
--- >>> (id <$> exampleRoseTree) == id exampleRoseTree
-- True
----------------------------------------------------------------------------------------------------
---------------------------------- QUESTION 2 - Introduction ---------------------------------------
{-
You're writing an app that queries the database for a list of items (products) and then performs
several transformation on these items, such as calculating the taxes, adding them to get the
total value, etc. The database query that returns the list of items might fail, so the result of
the query is wrapped in a Maybe.
-}
-- Type representing an item (product)
newtype Item a = Item
  { getItem :: (String, Sum a)
  } deriving (Eq, Show)

exampleItem :: Item Double
exampleItem = Item ("Phone", Sum 50.0)

-- Type of the result of the database query
type DBResponse = Maybe [Item Double]

-- Example of a database query result
dbResult :: DBResponse
dbResult = Just [Item ("Phone", Sum 50.0), Item ("Glasses", Sum 30.0)]

----------------------------------------------------------------------------------------------------
---------------------------------------- QUESTION 2 A ----------------------------------------------
{-
Write the Functor instance for Item.
-}
-- TODO
--cách 1
-- instance Functor Item where
--   fmap f (Item (name, Sum price)) =
--     Item {getItem = (name, Sum {getSum = f price})}
--cách 2
instance Functor Item where
  fmap f (Item (name, price)) = Item (name, fmap f price)

-- >>> fmap (*2) exampleItem
-- Item {getItem = ("Phone",Sum {getSum = 100.0})}
-- >>> fmap id exampleItem == id exampleItem
-- True
----------------------------------------------------------------------------------------------------
---------------------------------------- QUESTION 2 B ----------------------------------------------
{-
Write a function that gives you all the items of the list for free (price = 0.0).
-}
--cách 1
-- giveForFree :: DBResponse -> DBResponse
-- giveForFree (Just xs) =
--   Just (map (\(Item (name, _)) -> Item (name, Sum 0)) xs) -- TODO
--cách 2
giveForFree :: DBResponse -> DBResponse
giveForFree = fmap (map (\(Item (name, _)) -> Item (name, Sum 0)))

-- >>> giveForFree dbResult
-- Just [Item {getItem = ("Phone",Sum {getSum = 0.0})},Item {getItem = ("Glasses",Sum {getSum = 0.0})}]
----------------------------------------------------------------------------------------------------
---------------------------------------- QUESTION 2 C ----------------------------------------------
{-
Write a function that changes the products prices by applying a tax of 20%.
-}
--cách 1
-- applyTaxes :: DBResponse -> DBResponse
-- applyTaxes (Just xs) = Just (map (fmap (* 1.2)) xs) -- TODO
--cách 2
applyTaxes :: DBResponse -> DBResponse
applyTaxes = fmap (map (fmap (* 1.2)))

-- >>> applyTaxes dbResult
-- Just [Item {getItem = ("Phone",Sum {getSum = 60.0})},Item {getItem = ("Glasses",Sum {getSum = 36.0})}]
----------------------------------------------------------------------------------------------------
---------------------------------------- QUESTION 2 D ----------------------------------------------
{-
Write a function that marks the products as discounted (in the name) and applies the discount (that
goes from 0.0 to 1.0) to the price.
-}
--cách 1
-- markOnSale :: Double -> DBResponse -> DBResponse
-- markOnSale perc (Just xs) =
--   Just
--     (map
--        (\(Item (name, Sum price)) ->
--           Item
--             ( name ++ " (" ++ show (perc * 100) ++ "% Discount)"
--             , Sum ((1 - perc) * price)))
--        xs -- TODO
--      )
--cách 2
markOnSale :: Double -> DBResponse -> DBResponse
markOnSale perc =
  fmap
    (map
       (\(Item (name, price)) ->
          Item
            ( name ++ " (" ++ show (perc * 100) ++ "% Discount)"
            , fmap (* (1 - perc)) price)))

-- >>> markOnSale 0.3 dbResult
-- Just [Item {getItem = ("Phone (30.0% Discount)",Sum {getSum = 35.0})},Item {getItem = ("Glasses (30.0% Discount)",Sum {getSum = 21.0})}]
-- Just [Item {getItem = ("Phone (30.0% Discount)",Sum {getSum = 35.0})},Item {getItem = ("Glasses (30.0% Discount)",Sum {getSum = 21.0})}]
----------------------------------------------------------------------------------------------------
---------------------------------------- QUESTION 2 E ----------------------------------------------
{-
Write a function that returns a pair with the list of items as first argument and the total price
as second argument.
-}
--cách 1
-- listItemsWithFinalPrice :: DBResponse -> Maybe ([String], Double)
-- listItemsWithFinalPrice (Just xs) = Just (listName, sumPrice)
--   where
--     listName = map (\(Item (name, _)) -> name) xs
--     sumPrice = sum (map (\(Item (_, Sum price)) -> price) xs)
--cách 2
listItemsWithFinalPrice :: DBResponse -> Maybe ([String], Double)
listItemsWithFinalPrice =
  fmap
    (\xs -> (map (fst . getItem) xs, getSum . mconcat $ map (snd . getItem) xs))
-- >>> listItemsWithFinalPrice dbResult
-- Just (["Phone","Glasses"],80.0)
-- >>> listItemsWithFinalPrice . applyTaxes . markOnSale 0.3 $ dbResult
-- Just (["Phone (30.0% Discount)","Glasses (30.0% Discount)"],67.2)
