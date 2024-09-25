----------------------------------------------------------------------------------------------------
---------------------------------------- Question 1 ------------------------------------------------
{-
-- Write the specialized types of all the `Functor` and `Applicative` operators
-- in the `result` expression.
-}

eDiv :: Float -> Float -> Either String Float
eDiv x 0 = Left "division by zero"
eDiv x y = Right (x / y)



--                                               ?
--                                               |
result :: Either String Float --                 |
result = (\x y z -> x * y + z) <$> (3 `eDiv` 2) <*> Right 2 <*> pure 4
--                              |                            |_______________________________________________
--                              |                                                                           |
--                              ?                                                                           |
--                                                                                                          |
--                                                   _______________________________________________________|
--                                                  |
--                                                  ?



----------------------------------------------------------------------------------------------------
---------------------------------------- Question 2 ------------------------------------------------
{-
 - Implement all these Applicative functions
-}

-- Conditional execution of 'Applicative' expressions. For example,
-- > when debug (putStrLn "Debugging")
-- will output the string @Debugging@ if the Boolean value @debug@
-- is 'True', and otherwise do nothing.
when :: (Applicative f) => Bool -> f () -> f ()
-- when p s  = undefined
when p s  = if p then s else pure ()

-- The reverse of 'when'.
unless            :: (Applicative f) => Bool -> f () -> f ()
-- unless p s        =  undefined
unless p s = if p then pure () else s
-- unless p s = when (not p) s

-- Like 'replicateM', but discards the result.
replicateM_ :: (Applicative m) => Int -> m a -> m ()
-- replicateM_ n action = undefined
replicateM_ n action = if n <= 0 then pure () else action *> replicateM_ (n - 1) action


--------------------------------------------------------------------------------------------------------
-- NEXT LESSON IS GOING TO BE ABOUT USING APPLICATIVE ON A REAL WORLD PROBLEM. SO, WE'LL CUT IT HERE.
--------------------------------------------------------------------------------------------------------
