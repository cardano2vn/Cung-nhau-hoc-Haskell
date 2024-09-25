----------------------------------------------------------------------------------------------------
---------------------------------------- Question 1 ------------------------------------------------
{-
-- Write the specialized types of all the `Functor` and `Applicative` operators
-- in the `result` expression.
-}
eDiv :: Float -> Float -> Either String Float
eDiv x 0 = Left "division by zero"
eDiv x y = Right (x / y)

--                                               Either String (Float -> Float -> Float) -> Either String Float -> Either String (Float -> Float)
--                                               |
result :: Either String Float --                 |
result = (\x y z -> x * y + z) <$> (3 `eDiv` 2) <*> Right 2 <*> pure 4

--                              |                            |_______________________________________________
--                              |                                                                           |
--  (Float -> Float -> Float -> Float) -> Either String Float -> Either String (Float -> Float -> Float)                                                                      |
--                                                                                                          |
--                                                   _______________________________________________________|
--                                                  |
--                                                  Either String (Float -> Float) -> Either String Float -> Either String Float
----------------------------------------------------------------------------------------------------
---------------------------------------- Question 2 ------------------------------------------------
{-
 - Implement all these Applicative functions
-}
-- Conditional execution of 'Applicative' expressions. For example,
-- > when debug (putStrLn "Debugging")
-- will output the string @Debugging@ if the Boolean value @debug@
-- is 'True', and otherwise do nothing.
-- Conditional execution of 'Applicative' expressions.
when :: (Applicative f) => Bool -> f () -> f ()
when p s =
  if p
    then s
    else pure ()

-- The reverse of 'when'.
unless :: (Applicative f) => Bool -> f () -> f ()
unless p = when (not p)

-- Like 'replicateM', but discards the result.
replicateM_ :: (Applicative m) => Int -> m a -> m ()
replicateM_ n action
  | n < 0 = error "Number of repetitions must be non-negative."
  | n == 0 = pure ()
  | otherwise = action *> replicateM_ (n - 1) action
--------------------------------------------------------------------------------------------------------
-- NEXT LESSON IS GOING TO BE ABOUT USING APPLICATIVE ON A REAL WORLD PROBLEM. SO, WE'LL CUT IT HERE.
--------------------------------------------------------------------------------------------------------
