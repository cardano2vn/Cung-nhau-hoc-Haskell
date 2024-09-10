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

data RoseTree a = RoseNode a [RoseTree a] deriving (Eq, Show)

exampleRoseTree :: RoseTree Int
exampleRoseTree = RoseNode 1 [RoseNode 2 [], RoseNode 3 [RoseNode 4 []]]

-- TODO: Implement the Functor instance for RoseTree
instance Functor RoseTree where
    fmap f (RoseNode x children) = RoseNode (f x) (map (fmap f) children)

------------
-- in lesson
-- data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving (Show, Eq)

-- class Functor where
--      fmap :: (a -> b) -> fa -> fb

-- instance Functor Tree where
--      fmap f (leaf x) = Leaf (f x)
--      fmap f (Node lt x rt) = Node (fmap f lt) (f x) (fmap f rt) -- (<$> f lt)
------------

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
newtype Item a = Item {getItem :: (String, Sum a)} deriving (Eq, Show)

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
instance Functor Item where
    fmap f (Item (str, Sum a)) = Item (str, Sum (f a))

-- >>> fmap (*2) exampleItem
-- Item {getItem = ("Phone",Sum {getSum = 100.0})}

-- >>> fmap id exampleItem == id exampleItem
-- True

----------------------------------------------------------------------------------------------------
---------------------------------------- QUESTION 2 B ----------------------------------------------
{-
Write a function that gives you all the items of the list for free (price = 0.0).
-}

giveForFree :: DBResponse -> DBResponse
-- giveForFree = undefined -- TODO
giveForFree = fmap . map $ fmap (*0)

-- >>> 
-- Just [Item {getItem = ("Phone",Sum {getSum = 0.0})},Item {getItem = ("Glasses",Sum {getSum = 0.0})}]

----------------------------------------------------------------------------------------------------
---------------------------------------- QUESTION 2 C ----------------------------------------------
{-
Write a function that changes the products prices by applying a tax of 20%.
-}

applyTaxes :: DBResponse -> DBResponse
-- applyTaxes = undefined -- TODO
applyTaxes = fmap . map $ fmap (*1.2)

-- >>> applyTaxes dbResult
-- Just [Item {getItem = ("Phone",Sum {getSum = 60.0})},Item {getItem = ("Glasses",Sum {getSum = 36.0})}]

----------------------------------------------------------------------------------------------------
---------------------------------------- QUESTION 2 D ----------------------------------------------
{-
Write a function that marks the products as discounted (in the name) and applies the discount (that
goes from 0.0 to 1.0) to the price.
-}

markOnSale :: Double -> DBResponse -> DBResponse
-- markOnSale perc = undefined -- TODO
markOnSale perc = fmap . map $ \item ->
  let (name, Sum price) = getItem item
      discountedPrice = price * (1 - perc)
      discountedName = name ++ " (" ++ show (perc * 100) ++ "% Discount)"
  in Item (discountedName, Sum discountedPrice)

-- >>> markOnSale 0.3 dbResult
-- Just [Item {getItem = ("Phone (30.0% Discount)",Sum {getSum = 35.0})},Item {getItem = ("Glasses (30.0% Discount)",Sum {getSum = 21.0})}]

----------------------------------------------------------------------------------------------------
---------------------------------------- QUESTION 2 E ----------------------------------------------
{-
Write a function that returns a pair with the list of items as first argument and the total price
as second argument.
-}
-- dbResult = Just [Item ("Phone", Sum 50.0), Item ("Glasses", Sum 30.0)]
listItemsWithFinalPrice :: DBResponse -> Maybe ([String], Double)
-- listItemsWithFinalPrice = undefined -- TODO
listItemsWithFinalPrice (Just items) = Just (map (fst . getItem) items, sum (map (getSum . snd . getItem) items))
listItemsWithFinalPrice Nothing = Nothing

-- >>> listItemsWithFinalPrice dbResult
-- Just (["Phone","Glasses"],80.0)

-- >>> listItemsWithFinalPrice . applyTaxes . markOnSale 0.3 $ dbResult
-- Just (["Phone (30.0% Discount)","Glasses (30.0% Discount)"],67.2)
