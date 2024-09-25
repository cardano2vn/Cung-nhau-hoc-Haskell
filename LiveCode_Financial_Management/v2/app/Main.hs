-- import           Data.Maybe           (mapMaybe)
{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import           Control.Applicative  (Applicative (liftA2), (<|>))
import           Control.Exception    (SomeException (SomeException), try)
import           Control.Monad        (guard)
import           Data.Aeson           (FromJSON, ToJSON, eitherDecode, encode)
import           Data.Bifunctor       (bimap, first, second)
import qualified Data.ByteString.Lazy as B
import           Data.Either          (either, fromRight, lefts, rights)
import           Data.Foldable        (foldMap)
import           Data.List            (isInfixOf)
import           Data.List.Split      (splitOn)
import qualified Data.Map             as M
import           Data.Maybe           (fromMaybe, mapMaybe)
import           Data.Monoid          (Any (..), Sum (..))
import           Data.Time            (Day, defaultTimeLocale, parseTimeM)
import           Data.Time.Calendar   (Day, fromGregorian)
import           GHC.Generics         (Generic)
import           System.IO            (hFlush, stdout)
import           Text.Read            (readMaybe)

-- Error handling
data AppError
  = FileError String
  | ParseError String
  deriving (Show, Eq)

instance Semigroup AppError where
  FileError e1 <> FileError e2   = FileError (e1 <> "; " <> e2)
  ParseError e1 <> ParseError e2 = ParseError (e1 <> "; " <> e2)
  FileError e <> _               = FileError e
  _ <> FileError e               = FileError e

instance Monoid AppError where
  mempty = FileError ""

-- Priority Data Type
data Priority
  = High
  | Medium
  | Low
  deriving (Eq, Ord, Show)

-- Expense Data
data Expense = Expense
  { expenseId       :: String
  , expenseName     :: String
  , expenseAmount   :: Double
  , expenseCategory :: String
  , expenseDate     :: Day
  } deriving (Show, Generic)

instance ToJSON Expense

instance FromJSON Expense

-- Income Data
data Income = Income
  { incomeId     :: String
  , incomeSource :: String
  , incomeAmount :: Double
  , incomeDate   :: Day
  } deriving (Show, Generic)

instance FromJSON Income

instance ToJSON Income

-- Budget Data Type
data Budget = Budget
  { budgetCategory :: String
  , budgetAmount   :: Double
  } deriving (Show, Generic)

instance ToJSON Budget

instance FromJSON Budget

-- History Data
data History = History
  { historyDate   :: Day
  , totalExpenses :: Sum Double
  , totalIncomes  :: Sum Double
  } deriving (Show)

instance Semigroup History where
  History d e1 i1 <> History _ e2 i2 = History d (e1 <> e2) (i1 <> i2)

instance Monoid History where
  mempty = History (fromGregorian 1970 1 1) (Sum 0) (Sum 0)

-- loadFromFile :: FromJSON a => FilePath -> IO (Either AppError [a])
-- loadFromFile path = do
--   result <- try (B.readFile path) :: IO (Either SomeException B.ByteString)
--   return
--     $ case result of
--         Left err      -> Left (FileError (show err))
--         Right content -> either (Left . ParseError) Right (eitherDecode content)
-- File operations
loadFromFile :: FromJSON a => FilePath -> IO (Either AppError [a])
loadFromFile path =
  either (Left . FileError . show) (first ParseError . eitherDecode)
    <$> (try (B.readFile path) :: IO (Either SomeException B.ByteString))

saveToFile :: ToJSON a => FilePath -> [a] -> IO (Either AppError ())
saveToFile path content = Right . const () <$> B.writeFile path (encode content)

-- Generate new IDs
generateNewId :: [a] -> String -> String
generateNewId list inf = inf ++ show (length list + 1)

-- Convert String to Day
-- stringToDay :: String -> Maybe Day
-- stringToDay = parseTimeM True defaultTimeLocale "%Y-%m-%d"
-- import Data.Time (Day, parseTimeM, defaultTimeLocale)
stringToDay :: String -> Either String Day
stringToDay =
  maybe (Left "Invalid date format") Right
    . parseTimeM True defaultTimeLocale "%Y-%m-%d"

-- Parsing
-- parseExpense :: String -> Either String Expense
-- parseExpense str =
--   case splitOn [','] str of
--     [name, amount, category, dateStr] ->
--       case stringToDay dateStr of
--         Just date ->
--           Right
--             Expense
--               { expenseId = ""
--               , expenseName = name
--               , expenseAmount = read amount
--               , expenseCategory = category
--               , expenseDate = date
--               }
--         Nothing -> Left "Invalid date format"
--     _ -> Left "Invalid expense format"
parseExpense :: String -> Either String Expense
parseExpense str =
  bimap
    (const "Invalid expense format")
    (\[name, amount, category, dateStr] ->
       Expense
         ""
         name
         (read amount)
         category
         (fromRight (error "Invalid date") (stringToDay dateStr)))
    (parseFields str)
  where
    parseFields s =
      case splitOn [','] s of
        [name, amount, category, dateStr] ->
          Right [name, amount, category, dateStr]
        _ -> Left ()

-- addExpenseToFile :: Expense -> IO (Either AppError ())
-- addExpenseToFile expense = do
--   result <- loadFromFile "expenses.json"
--   case result of
--     Left err -> return $ Left err
--     Right exps -> do
--       let updatedExpenses = expense {expenseId = generateNewId exps "E"} : exps
--       saveToFile "expenses.json" updatedExpenses
--       return $ Right ()
addToFile ::
     (ToJSON a, FromJSON a)
  => FilePath
  -> (a -> [a] -> [a])
  -> a
  -> IO (Either AppError ())
addToFile path f newItem =
  loadFromFile path >>= either (pure . Left) (saveToFile path . f newItem)

-- Add expenses
addExpenseToFile :: Expense -> IO (Either AppError ())
addExpenseToFile =
  addToFile
    "expenses.json"
    (\e exps -> e {expenseId = generateNewId exps "E"} : exps)

-- Add incomes
-- addIncomeToFile :: Income -> IO (Either AppError ())
-- addIncomeToFile =
--   addToFile
--     "incomes.json"
--     (\i incs -> i {incomeId = generateNewId incs "I"} : incs)
-- Handle Results
handleResults :: [Either AppError ()] -> IO ()
handleResults results =
  putStrLn
    $ if mempty == mconcat (lefts results)
        then "All operations succeeded!"
        else "Errors occurred: " ++ show (mconcat (lefts results))

-- addMultipleExpenses :: [Expense] -> IO ()
-- addMultipleExpenses expenses = do
--   results <- mapM addExpenseToFile expenses
--   handleResults results
-- handleResults :: [Either AppError ()] -> IO ()
-- handleResults results =
--   let errors = lefts results
--       hasErrors = getAny $ foldMap (Any . isLeft) results
--       message =
--         if hasErrors
--           then "Errors occurred: " ++ show errors
--           else "All operations succeeded!"
--    in putStrLn message
-- Add Multiple
addMultiple :: (a -> IO (Either AppError ())) -> [a] -> IO ()
addMultiple addFn items = handleResults =<< mapM addFn items

-- Collect lines until an empty line is entered
-- collectLines :: IO [String]
-- collectLines = do
--   line <- getLine
--   if null line
--     then return []
--     else (line :) <$> collectLines
collectLines :: IO [String]
collectLines =
  getLine >>= \line ->
    if null line
      then pure []
      else (line :) <$> collectLines

-- generateHistory :: [Expense] -> [Income] -> M.Map Day History
-- generateHistory expenses incomes =
--   let expenseHistory =
--         map
--           (\e ->
--              ( expenseDate e
--              , History (expenseDate e) (Sum $ expenseAmount e) mempty))
--           expenses
--       incomeHistory =
--         map
--           (\i ->
--              ( incomeDate i
--              , History (incomeDate i) mempty (Sum $ incomeAmount i)))
--           incomes
--       combined =
--         M.unionWith
--           (<>)
--           (M.fromListWith (<>) expenseHistory)
--           (M.fromListWith (<>) incomeHistory)
--    in combined
-- Generate History
generateHistory :: [Expense] -> [Income] -> M.Map Day History
generateHistory exps incs =
  M.unionWith
    (<>)
    (M.fromListWith
       (<>)
       [ (expenseDate e, History (expenseDate e) (Sum $ expenseAmount e) mempty)
       | e <- exps
       ])
    (M.fromListWith
       (<>)
       [ (incomeDate i, History (incomeDate i) mempty (Sum $ incomeAmount i))
       | i <- incs
       ])

-- Print History
-- printHistory :: FilePath -> FilePath -> IO ()
-- printHistory expensesPath incomesPath = do
--   expensesResult <- loadFromFile expensesPath
--   incomesResult <- loadFromFile incomesPath
--   case (expensesResult, incomesResult) of
--     (Right expenses, Right incomes) -> do
--       let history = generateHistory expenses incomes
--       putStrLn "Financial History:"
--       mapM_
--         (\(date, report) ->
--            putStrLn
--              $ show date
--                  ++ ": Expenses: "
--                  ++ show (getSum $ totalExpenses report)
--                  ++ ", Incomes: "
--                  ++ show (getSum $ totalIncomes report))
--         (M.toList history)
--     (Left err, _) -> putStrLn $ "Error loading expenses: " ++ show err
--     (_, Left err) -> putStrLn $ "Error loading incomes: " ++ show err
printHistory :: FilePath -> FilePath -> IO ()
printHistory expPath incPath =
  liftA2 (,) (loadFromFile expPath) (loadFromFile incPath) >>= \case
    (Right exps, Right incs) ->
      mapM_ (putStrLn . formatHistory) (M.toList $ generateHistory exps incs)
    (Left err, _) -> putStrLn $ "Error loading expenses: " ++ show err
    (_, Left err) -> putStrLn $ "Error loading incomes: " ++ show err
  where
    formatHistory (date, History _ (Sum e) (Sum i)) =
      show date ++ ": Expenses: " ++ show e ++ ", Incomes: " ++ show i

-- Search Expenses
-- searchExpenses :: String -> IO ()
-- searchExpenses query =
--   loadFromFile "expenses.json" >>= \case
--     Left err -> putStrLn $ "Error: " ++ show err
--     Right exps -> mapM_ (putStrLn . formatExpense) (filter (matches query) exps)
--   where
--     matches q e = isInfixOf q (expenseName e) || q `isInfixOf` expenseCategory e
--     formatExpense e = expenseName e ++ " : " ++ show (expenseAmount e)
searchExpenses :: String -> IO ()
searchExpenses query =
  loadFromFile "expenses.json"
    >>= either
          (putStrLn . ("Error: " ++) . show)
          (mapM_ (putStrLn . formatExpense) . filter matches)
  where
    matches e =
      query `isInfixOf` expenseName e || query `isInfixOf` expenseCategory e
    formatExpense e = expenseName e ++ " : " ++ show (expenseAmount e)

-- expensesByCategory :: [Expense] -> M.Map String Double
-- expensesByCategory = M.map getSum . foldr updateCategory M.empty
--   where
--     updateCategory expense acc =
--       let category = expenseCategory expense
--           amount = Sum $ expenseAmount expense
--        in M.insertWith (<>) category amount acc
-- -- Expense by Category
expenseByCategory :: [Expense] -> M.Map String Double
expenseByCategory =
  M.map getSum
    . foldr
        (\e -> M.insertWith (<>) (expenseCategory e) (Sum $ expenseAmount e))
        M.empty

-- expensesByCategory :: [Expense] -> M.Map String Double
-- expensesByCategory =
--   foldr (M.insertWith (+) <$> expenseCategory <*> expenseAmount) M.empty
-- Check Budgets
-- checkBudgets :: [Expense] -> [Budget] -> M.Map String String
-- checkBudgets exps bgs =
--   M.fromList $ mapMaybe check (M.toList totalExpensesByCategory)
--   where
--     totalExpensesByCategory = expenseByCategory exps
--     budgetMap = M.fromList [(budgetCategory b, budgetAmount b) | b <- bgs]
--     check (cat, amt) =
--       case M.lookup cat budgetMap of
--         Just budget
--           | amt > budget ->
--             Just (cat, cat ++ " exceeds budget by " ++ show (amt - budget))
--         _ ->
--           case M.lookup "Utilities" budgetMap of
--             Just budget
--               | amt > budget ->
--                 Just (cat, cat ++ " exceeds budget by " ++ show (amt - budget))
--             _ -> Nothing
checkBudgets :: [Expense] -> [Budget] -> M.Map String String
checkBudgets exps bgs =
  M.fromList $ mapMaybe check (M.toList totalExpensesByCategory)
  where
    totalExpensesByCategory = expenseByCategory exps
    budgetMap = M.fromList [(budgetCategory b, budgetAmount b) | b <- bgs]
    check (cat, amt) =
      maybeBudget >>= \b ->
        if amt > b
          then Just (cat, cat ++ " exceeds budget by " ++ show (amt - b))
          else Nothing
      where
        maybeBudget = M.lookup cat budgetMap <|> M.lookup "Utilities" budgetMap

-- class Applicative f => Alternative f where
--     empty :: f a
--     (<|>) :: f a -> f a -> f a
-- Checked Budgets
-- checkedBudgets :: FilePath -> FilePath -> IO ()
-- checkedBudgets expPath bgPath = do
--   expsResult <- loadFromFile expPath
--   bgsResult <- loadFromFile bgPath
--   case (expsResult, bgsResult) of
--     (Right exps, Right bgs) ->
--       mapM_
--         putStrLn
--         (M.elems (checkBudgets exps bgs)
--            ++ ["All expenses are within the budget!"])
--     (Left err, _) -> putStrLn $ "Error loading expenses: " ++ show err
--     (_, Left err) -> putStrLn $ "Error loading budgets: " ++ show err
checkedBudgets :: FilePath -> FilePath -> IO ()
checkedBudgets expPath bgPath =
  liftA2 (,) (loadFromFile expPath) (loadFromFile bgPath) >>= \case
    (Right exps, Right bgs) ->
      mapM_ putStrLn
        $ M.elems (checkBudgets exps bgs)
            ++ ["All expenses are within the budget!"]
    (Left err, _) -> putStrLn $ "Error loading expenses: " ++ show err
    (_, Left err) -> putStrLn $ "Error loading budgets: " ++ show err

-- Main Menu
mainMenu :: IO ()
mainMenu = do
  putStrLn "===== Personal Finance Management ====="
  putStrLn "1. Add Expenses"
  putStrLn "2. Add Incomes"
  putStrLn "3. Print History"
  putStrLn "4. Search Expenses"
  putStrLn "5. Check Budgets"
  putStrLn "6. Exit"
  putStr "Choose an option: "
  hFlush stdout
  option <- getLine
  case option of
    "1" -> do
      putStrLn
        "Enter Expense Details (Name,Amount,Category,Date) or an empty line to finish:"
      expenseLines <- collectLines
      let expenses = rights $ map parseExpense expenseLines
      addMultiple addExpenseToFile expenses
      mainMenu
    "3" -> do
      printHistory "expenses.json" "incomes.json"
      mainMenu
    "4" -> do
      putStrLn "Enter search query: "
      query <- getLine
      searchExpenses query
      mainMenu
    "5" -> do
      checkedBudgets "expenses.json" "budgets.json"
      mainMenu
    "6" -> putStrLn "Exiting..."
    _ -> do
      putStrLn "Invalid option. Please try again."
      mainMenu
