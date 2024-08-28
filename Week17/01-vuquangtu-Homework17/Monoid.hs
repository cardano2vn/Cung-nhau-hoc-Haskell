-- import           Data.Monoid (Monoid)
--------------------------------------------------------------------------------------------
----------------------------------------- Boolean ------------------------------------------
{-
 - Define all possible Monoid instances for `Bool` same as we did for `Int` and `Ord`
 - in the lesson.
 -}
-- import           Data.Semigroup (Semigroup)
newtype All = All
  { getAll :: Bool
  } deriving (Show, Eq)

newtype Any = Any
  { getAny :: Bool
  } deriving (Show, Eq)

-- TODO: Define the Semigroup and Monoid instances for All and Any
-- Test it out
instance Semigroup All where
  (<>) all1 all2 = All {getAll = getAll all1 && getAll all2}

instance Monoid All where
  mempty = All {getAll = True}

instance Semigroup Any where
  (<>) any1 any2 = Any {getAny = getAny any1 || getAny any2}

instance Monoid Any where
  mempty = Any {getAny = False}

--   (<>) (All {getAll = a}) (All {getAll = b}) = (All {getAll = a && b})
--- >>> All True <> All False <> All True
-- All {getAll = False}

-- All {getAll = False}
--- >>> (All False <> All True) <> All True == All False <> (All True <> All True)
-- True

-- True
--- >>> Any True <> Any False <> Any True
-- Any {getAny = True}

-- Any {getAny = True}
--- >>> (Any False <> Any True) <> Any True == Any False <> (Any True <> Any True)
-- True
--- >>> All True <> mempty == All True
-- True

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
newtype Log =
  Log [String]
  deriving (Show, Eq)

-- TODO: Define the Semigroup and Monoid instances for Log
-- Test it out
instance Semigroup Log where
  (<>) (Log xs) (Log ys) = Log (xs ++ ys)

instance Monoid Log where
  mempty = Log []

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
data Config = Config
  { port :: Int
  , host :: String
  } deriving (Show, Eq)

-- TODO: Define the Semigroup and Monoid instances for Config
-- Test it out
c1, c2, c3 :: Config
c1 = Config {port = 8080, host = "localhost"}

c2 = Config {port = 3000, host = "localhost"}

c3 = Config {port = 4000, host = "example.com"}

instance Semigroup Config where
  (<>) Config {port = p1, host = h1} Config {port = p2, host = h2} =
    Config
      { port =
          if p2 /= 0
            then p2
            else p1
      , host =
          if h2 /= ""
            then h2
            else h1
      }

instance Monoid Config where
  mempty = Config {port = 0, host = ""}

--- >>> c1 <> c2
-- Config {port = 3000, host = "localhost"}

-- Config {port = 3000, host = "localhost"}
--- >>> c1 <> c2 <> c3
-- Config {port = 4000, host = "example.com"}

-- Config {port = 4000, host = "example.com"}
--- >>> mconcat [c1, c2, c3]
-- Config {port = 4000, host = "example.com"}

-- Config {port = 4000, host = "example.com"}
--- >>> mempty <> c3
-- Config {port = 4000, host = "example.com"}

-- Config {port = 4000, host = "example.com"}
--- >>> c3 <> mempty
-- Config {port = 4000, host = "example.com"}

-- Config {port = 4000, host = "example.com"}
--- >>> (c1 <> c2) <> c3 == c1 <> (c2 <> c3)
-- True

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
data Severity
  = Low
  | Medium
  | High
  | Critical
  deriving (Show, Eq, Ord)

-- TODO: Define the Semigroup and Monoid instances for Severity
-- TODO: Define the Emergency type
-- TODO: Define the Semigroup and Monoid instances for Emergency
-- Test it out
instance Semigroup Severity where
  (<>) Critical _ = Critical
  (<>) _ Critical = Critical
  (<>) High _     = High
  (<>) _ High     = High
  (<>) Medium _   = Medium
  (<>) _ Medium   = Medium
  (<>) _ _        = Low

instance Monoid Severity where
  mempty = Low

data Emergency = Emergency
  { severity    :: Severity
  , description :: String
  , resolved    :: All
  } deriving (Show, Eq)

instance Semigroup Emergency where
  (<>) (Emergency s1 d1s (All b1)) (Emergency s2 d2s (All b2)) =
    Emergency (s1 <> s2) (d1s ++ d2s) (All (b1 && b2))

instance Monoid Emergency where
  mempty = Emergency Low "" (All True)

e1, e2, e3, e4 :: Emergency
e1 = Emergency Low "You left the lights on; " (All True)

e2 =
  Emergency
    Medium
    "You might have left the stove on, but you're not sure; "
    (All False)

e3 = Emergency High "The building is on fire; " (All True)

e4 = Emergency Critical "You mother-in-law is coming over!; " (All False)
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
-- True
-- True
-- True
