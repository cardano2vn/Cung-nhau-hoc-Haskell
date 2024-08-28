import Data.Semigroup
import Data.Monoid

--------------------------------------------------------------------------------------------
----------------------------------------- Boolean ------------------------------------------
{-
 - Define all possible Monoid instances for `Bool` same as we did for `Int` and `Ord`
 - in the lesson.
 -}

newtype All' = All' { getAll :: Bool } deriving (Show, Eq)
newtype Any' = Any' { getAny :: Bool } deriving (Show, Eq)

-- TODO: Define the Semigroup and Monoid instances for All and Any
-- class Semigroup a where
--     (<>) :: a -> a -> a

instance Semigroup All' where
    All' True <> All' True = All' True
    All' False <> _ = All' False
    _ <> All' False = All' False

instance Semigroup Any' where
    Any' False <> Any' False = Any' False
    Any' True <> _ = Any' True
    _ <> Any' True = Any' True

-- class Semigroup a => Monoid a where -- identity
--     mempty :: a
--     mappend :: a -> a -> a -- <>
--     mconcat :: [a] -> a -- foldr <> mempty

instance Monoid All' where
    mempty = All' True

instance Monoid Any' where
    mempty = Any' False

-- Test it out

--- >>> All True <> All False <> All True
-- All {getAll = False}

--- >>> (All False <> All True) <> All True == All False <> (All True <> All True)
-- True

--- >>> Any True <> Any False <> Any True
-- Any {getAny = True}

--- >>> (Any False <> Any True) <> Any True == Any False <> (Any True <> Any True)
-- True

--- >>> All True <> mempty == All True
-- True

--- >>> mempty <> All True == All True
-- True

--- >>> Any False <> mempty == Any False
-- True

--- >>> mempty <> Any False == Any False
-- True

--- >>> mconcat [All True, All False, All True] == foldr (<>) mempty [All True, All False, All True]
-- True

--------------------------------------------------------------------------------------------
-------------------------------------------- Log --------------------------------------------
{-
 - Suppose you have a system where you want to collect logs from various parts of your application.
 - You could define a Log type and make it a Monoid, where mempty is an empty log and mappend
 - combines two logs.
 -}

newtype Log = Log [String] deriving (Show, Eq)

-- TODO: Define the Semigroup and Monoid instances for Log
instance Semigroup Log where
    (<>) (Log []) (Log ys) = Log ys
    (<>) (Log (x:xs)) (Log ys) = Log (x : xs <> ys)

instance Monoid Log where 
    mempty = Log []

-- Test it out
log1, log2, log3 :: Log
log1 = Log ["Hello"]
log2 = Log ["World"]
log3 = Log ["!"]

--- >>> log1 <> log2
-- Log ["Hello","World"]

--- >>> mconcat [log1, log2, log3]
-- Log ["Hello","World","!"]

--- >>> mconcat [log1, log2, log3] == log1 <> log2 <> log3
-- True

--- >>> mempty <> log1 == log1
-- True

--- >>> log1 <> mempty == log1
-- True

--- >>> (log1 <> log2) <> log3 == log1 <> (log2 <> log3)
-- True

--- >>> mconcat [log1, log2, log3] == foldr (<>) mempty [log1, log2, log3]
-- True

--------------------------------------------------------------------------------------------
------------------------------------------ Config ------------------------------------------
{-
- Suppose you have a configuration that can be overridden.
- You could define a Config type and make it a Monoid, where mempty is a default configuration
- and mappend overrides the configuration.
-}

data Config = Config { port :: Int, host :: String } deriving (Show, Eq)

-- TODO: Define the Semigroup and Monoid instances for Config
instance Semigroup Config where
    (<>) (Config p h) (Config 0 []) = Config p h
    (<>) (Config 0 []) (Config p h) = Config p h    
    (<>) (Config p1 h1) (Config p2 h2) = Config p2 h2
    
instance Monoid Config where
    mempty = Config 0 []

-- Test it out

c1, c2, c3 :: Config
c1 = Config { port = 8080, host = "localhost" }
c2 = Config { port = 3000, host = "localhost" }
c3 = Config { port = 4000, host = "example.com" }

--- >>> c1 <> c2
-- Config {port = 3000, host = "localhost"}

--- >>> c1 <> c2 <> c3
-- Config {port = 4000, host = "example.com"}

--- >>> mconcat [c1, c2, c3]
-- Config {port = 4000, host = "example.com"}

--- >>> mempty <> c3
-- Config {port = 4000, host = "example.com"}

--- >>> c3 <> mempty
-- Config {port = 4000, host = "example.com"}

--- >>> (c1 <> c2) <> c3 == c1 <> (c2 <> c3)
-- True

--- >>> mconcat [c1, c2, c3] == foldr (<>) mempty [c1, c2, c3]
-- True

--------------------------------------------------------------------------------------------
---------------------------------------- Emergency -----------------------------------------
{-
 - Suppose you have a system that can raise emergencies of various severities.
 - We'll use the same `Severity` type as in the lesson, but now it's going to be part of the
 - `Emergency` type. The `Emergency` type has to contain:
  - 1. A Severity
  - 2. A description of the emergency
  - 3. A flag indicating whether the emergency has been resolved
 - Once you define the type, make it a Monoid.
 -}

data Severity = Low | Medium | High | Critical deriving (Show, Eq)

-- TODO: Define the Semigroup and Monoid instances for Severity
instance Semigroup Severity where
    (<>) Critical _ = Critical
    (<>) _ Critical = Critical
    (<>) High _ = High
    (<>) _ High = High
    (<>) Medium _ = Medium
    (<>) _ Medium = Medium
    (<>) _ _ = Low

instance Monoid Severity where
    mempty = Low

-- TODO: Define the Emergency type
data Emergency = Emergency {serverity ::Severity, description :: String, flag :: All'} deriving (Show, Eq)

-- TODO: Define the Semigroup and Monoid instances for Emergency
instance Semigroup Emergency where
    (<>) (Emergency a b (All' c)) (Emergency x y (All' z)) = Emergency (a <> x) (b++y) (All' c <> All' z)
    
instance Monoid Emergency where
    mempty = Emergency Low "" (All' True)

-- Test it out

e1, e2, e3, e4 :: Emergency
e1 = Emergency Low "You left the lights on; " (All' True)
e2 = Emergency Medium "You might have left the stove on, but you're not sure; " (All' False)
e3 = Emergency High "The building is on fire; " (All' True)
e4 = Emergency Critical "You mother-in-law is coming over!; " (All' False)

--- >>> e1 <> e2
-- Emergency {severity = Medium, description = "You left the lights on; You might have left the stove on, but you're not sure; ", resolved = All {getAll = False}}

--- >>> e1 <> e2 <> e3 <> e4
-- Emergency {severity = Critical, description = "You left the lights on; You might have left the stove on, but you're not sure; The building is on fire; You mother-in-law is coming over!; ", resolved = All {getAll = False}}

--- >>> e1 <> e2 <> e3 <> e4 == mconcat [e1, e2, e3, e4]
-- True

--- >>> mempty <> e4
-- Emergency {severity = Critical, description = "You mother-in-law is coming over!; ", resolved = All {getAll = False}}

--- >>> e4 <> mempty
-- Emergency {severity = Critical, description = "You mother-in-law is coming over!; ", resolved = All {getAll = False}}

--- >>> e3 <> mempty == e3
-- True

--- >>> mempty <> e3 == e3
-- True

--- >>> (e1 <> e2) <> e3 == e1 <> (e2 <> e3)
-- True

--- >>> mconcat [e1, e2, e3, e4] == foldr (<>) mempty [e1, e2, e3, e4]
-- True
