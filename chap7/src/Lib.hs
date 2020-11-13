{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module Lib where

import Control.Monad
import Control.Monad.Logic (Logic, MonadLogic (interleave, (>>-)))
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer (MonadWriter, Writer, WriterT, execWriter, execWriterT, runWriter, tell)
import Data.List (unfoldr)
import Data.Monoid (Sum (..))
import Data.Set (Set)
import qualified Data.Set as S

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- data TimeMachine = TimeMachine
--   { _manufacturer :: Manufacturer,
--     _tname :: Name,
--     _direction :: Direction,
--     _price :: Price
--   }
--   deriving (Show)

-- newtype Manufacturer = Manufacturer String deriving (Show)

-- newtype Model = Model Int deriving (Show)

-- newtype Name = Name String deriving (Show)

-- data Direction = PAST | FUTURE deriving (Show)

-- data Price = Price {_value :: Double} deriving (Show)

-- Exercise 7-1

brokenThreeJumps :: Int -> [Int]
brokenThreeJumps x = do
  f <- jumps
  g <- jumps
  h <- jumps
  return $ x + f + g + h
  where
    jumps = [(-1), 3, 5]

brokenJumps :: Int -> Int -> [Int]
brokenJumps _ 0 = []
brokenJumps x n = do
  f <- [(-1), 3, 5]
  (x + f) : brokenJumps x (n - 1)

broken1 :: Integer -> [Integer]
broken1 n = [n - 1, n + 1]

broken2 :: Integer -> [Integer]
broken2 n = [1024, n + 2]

-- Exercise 7-2

find_ :: (a -> Bool) -> [a] -> Maybe a
find_ f xs = msum $ map (\x -> if f x then return x else Nothing) xs

----

data Client
  = GovOrg {clientName :: String}
  | Company {clientName :: String, person :: Person, duty :: String}
  | Individual {person :: Person}
  deriving (Eq, Ord, Show)

data Person = Person {firstName :: String, lastName :: String, gender :: Gender}
  deriving (Eq, Ord, Show)

data ClientKind
  = KindGovOrg
  | KindCompany
  | KindIndividual
  deriving (Eq, Ord, Show)

data Gender
  = Male
  | Female
  | UnknwonGender
  deriving (Eq, Ord, Show)

--- Products

data Product = Product {productId :: Integer, productType :: ProductType}
  deriving (Eq, Ord, Show)

data ProductType
  = TimeMachine
  | TravelGuide
  | Tool
  | Trip
  | Camera
  deriving (Eq, Ord, Show)

--- Purchase in the DB

data Purchase = Purchase {client :: Client, products :: [Product]}
  deriving (Eq, Ord, Show)

data PurchaseInfo
  = InfoClientKind ClientKind
  | InfoClientDuty String
  | InfoClientGender Gender
  | InfoPurchasedProduct Integer
  | InfoPurchasedProductType ProductType
  deriving (Show, Eq, Ord)

newtype Transaction = Transaction (Set PurchaseInfo)
  deriving (Eq, Ord)

productsToPurchaseInfo :: [Product] -> Set PurchaseInfo
productsToPurchaseInfo =
  foldr
    ( \(Product i t) pInfos ->
        S.insert (InfoPurchasedProduct i) $ S.insert (InfoPurchasedProductType t) pInfos
    )
    S.empty

purchaseToTransaction :: Purchase -> Transaction
purchaseToTransaction (Purchase c prods) =
  Transaction $ clientToPuchaseInfo c `S.union` productsToPurchaseInfo prods

-- Exercise 7-3

clientToPuchaseInfo :: Client -> Set PurchaseInfo
clientToPuchaseInfo (GovOrg _) = S.fromList [InfoClientKind KindGovOrg]
clientToPuchaseInfo (Company _ _ d) = S.fromList [InfoClientKind KindCompany, InfoClientDuty d]
clientToPuchaseInfo (Individual (Person {gender})) = S.fromList [InfoClientKind KindIndividual, InfoClientGender gender]

---

-- Transaction : e.g. a customer purchased a set of products e.g. bread, milk
-- From these transactions, we intend to create association rules
-- e.g. { bread, milk } => egg , meaning:
-- if someone buys bread and milk, they bought eggs, so for futur customers, if they have { bread, milk } in their cart
-- we can recommend eggs to them based on this assoc rule.
-- ++ Support ++ : The support of a set of items is defined as total transactions for the set / total transactions
-- ++ confidence ++ : Supp (A => C) = Supp (A and C) / Supp(A)

newtype FrequentSet = FrequentSet (Set PurchaseInfo)
  deriving (Eq, Ord, Show)

data AssocRule = AssocRule (Set PurchaseInfo) (Set PurchaseInfo)
  deriving (Eq, Ord)

instance Show AssocRule where
  show (AssocRule a b) = show a ++ " => " ++ show b

setSupport :: [Transaction] -> FrequentSet -> Double
setSupport trans (FrequentSet setPInfo) =
  let total = length trans
      isSubsetOfTrans :: Transaction -> Bool
      isSubsetOfTrans (Transaction tElts) = setPInfo `S.isSubsetOf` tElts
      supp = length (filter isSubsetOfTrans trans)
   in fromIntegral supp / fromIntegral total

ruleConfidence :: [Transaction] -> AssocRule -> Double
ruleConfidence trans (AssocRule a b) =
  setSupport trans (FrequentSet $ a `S.union` b) / setSupport trans (FrequentSet a)

generateL1 :: Double -> [Transaction] -> [FrequentSet]
generateL1 minSupport transactions =
  noDups $ do
    Transaction t <- transactions
    e <- S.toList t
    let fs = FrequentSet $ S.singleton e
    guard $ setSupport transactions fs > minSupport
    return fs

noDups :: Ord a => [a] -> [a]
noDups = S.toList . S.fromList

generateNextLK :: Double -> [Transaction] -> (Int, [FrequentSet]) -> Maybe ([FrequentSet], (Int, [FrequentSet]))
generateNextLK _ _ (_, []) = Nothing
generateNextLK minSupport transactions (k, lk) =
  let lk1 = noDups $ do
        FrequentSet a <- lk
        FrequentSet b <- lk
        guard $ S.size (a `S.intersection` b) == k - 1
        let fs = FrequentSet $ a `S.union` b
        guard $ setSupport transactions fs > minSupport
        return fs
   in Just (lk1, (k + 1, lk1))

generateAssocRules :: Double -> [Transaction] -> [FrequentSet] -> [AssocRule]
generateAssocRules minConfidence transactions sets = do
  FrequentSet fs <- sets
  subset@(_ : _) <- powerset $ S.toList fs
  let ssubset = S.fromList subset
      rule = AssocRule ssubset (fs `S.difference` ssubset)
  guard $ ruleConfidence transactions rule > minConfidence
  return rule

powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x : xs) = powerset xs ++ map (x :) (powerset xs)

apriori :: Double -> Double -> [Transaction] -> [AssocRule]
apriori minSupport minConfidence transactions =
  generateAssocRules minConfidence transactions $
    concat $ unfoldr (generateNextLK minSupport transactions) (1, generateL1 minSupport transactions)

--- Example dataset

-------

indiv = Individual (Person "Ali" "Ahmed" Male)

p1 = Product 1 TimeMachine

p2 = Product 2 TravelGuide

p3 = Product 3 Tool

p4 = Product 4 Trip

p5 = Product 5 Camera

t1 = purchaseToTransaction (Purchase indiv [p1, p2, p5])

t2 = purchaseToTransaction (Purchase indiv [p2, p4])

t3 = purchaseToTransaction (Purchase indiv [p2, p3])

t4 = purchaseToTransaction (Purchase indiv [p1, p2, p4])

t5 = purchaseToTransaction (Purchase indiv [p1, p3])

t6 = purchaseToTransaction (Purchase indiv [p2, p3])

t7 = purchaseToTransaction (Purchase indiv [p1, p3])

t8 = purchaseToTransaction (Purchase indiv [p1, p2, p3, p5])

t9 = purchaseToTransaction (Purchase indiv [p1, p2, p3])

listOfTransactions = [t1, t2, t3, t4, t5, t6, t7, t8, t9]

------

paths1 :: [(Int, Int)] -> Int -> Int -> [[Int]]
paths1 edges start end = do
  (e_start, e_end) <- edges
  guard $ e_start == start
  subpath <- paths1 edges e_end end
  return $ start : subpath

ps :: [(Int, Int)]
ps = [(2013, 501), (2013, 1004), (501, 2558), (1004, 2558)]

ps2 :: [(Int, Int)]
ps2 = [(2013, 501), (501, 2558), (501, 1004), (1004, 501), (2013, 2558)]

paths2 :: [(Int, Int)] -> Int -> Int -> [[Int]]
paths2 edges start end =
  let e_paths = do
        (e_start, e_end) <- edges
        guard $ e_start == start
        subpath <- paths2 edges e_end end
        return $ start : subpath
   in if start == end
        then return [end] `mplus` e_paths
        else e_paths

-- writing paths using Logic Monad

pathsL :: [(Int, Int)] -> Int -> Int -> Logic [Int]
pathsL edges start end =
  let e_paths = do
        (e_start, e_end) <- choices edges
        guard $ e_start == start
        subpath <- pathsL edges e_end end
        return $ start : subpath
   in if start == end
        then return [end] `mplus` e_paths
        else e_paths

choices :: [a] -> Logic a
choices = msum . map return

pathsL2 :: [(Int, Int)] -> Int -> Int -> Logic [Int]
pathsL2 edges start end =
  let e_paths =
        choices edges
          >>= \(e_start, e_end) ->
            guard (e_start == start)
              >> pathsL edges e_end end
                >>= \subpath ->
                  return $ start : subpath
   in if start == end
        then return [end] `mplus` e_paths
        else e_paths

pathsLFair :: [(Int, Int)] -> Int -> Int -> Logic [Int]
pathsLFair edges start end =
  let e_paths =
        choices edges >>- \(e_start, e_end) ->
          guard (e_start == start)
            >> pathsLFair edges e_end end >>- \subpath ->
              return $ start : subpath
   in if start == end
        then return [end] `interleave` e_paths
        else e_paths

-- Combining values under a Monad
addPrefix :: String -> Reader String String
addPrefix s = ask >>= (\p -> return $ p ++ s)

addPrefixL :: [String] -> Reader String [String]
addPrefixL = mapM addPrefix

logInformation :: [String] -> Writer String ()
logInformation = mapM_ (\s -> tell (s ++ "\n"))

sequence2 :: Monad m => [m a] -> m [a]
sequence2 [] = return []
sequence2 (x : xs) = x >>= \a -> (a :) <$> sequence2 xs

mapM2 :: Monad m => (a -> m b) -> [a] -> m [b]
mapM2 _ [] = return []
mapM2 f (x : xs) = f x >>= \b -> (b :) <$> mapM2 f xs

factorialSteps :: Integer -> Writer (Sum Integer) Integer
factorialSteps n = foldM (\f x -> tell (Sum 1) >> return (f * x)) 1 [1 .. n]

powerset2 :: [a] -> [[a]]
powerset2 = filterM (\_ -> [False, True])

--

pathsWriter :: [(Int, Int)] -> Int -> Int -> [[Int]]
pathsWriter edges start end = map execWriter (pathsWriter' edges start end)

pathsWriter' :: [(Int, Int)] -> Int -> Int -> [Writer [Int] ()]
pathsWriter' edges start end =
  let e_paths = do
        (e_start, e_end) <- edges
        guard $ e_start == start
        subpath <- pathsWriter' edges e_end end
        return $ do
          tell [start]
          subpath
   in if start == end
        then tell [start] : e_paths
        else e_paths

pathsWriterT' :: [(Int, Int)] -> Int -> Int -> WriterT [Int] [] ()
pathsWriterT' edges start end =
  let e_paths = do
        (e_start, e_end) <- lift edges
        guard $ e_start == start
        tell [start]
        pathsWriterT' edges e_end end
   in if start == end
        then tell [start] `mplus` e_paths
        else e_paths

pathsWriterT :: [(Int, Int)] -> Int -> Int -> [[Int]]
pathsWriterT edges start end = execWriterT (pathsWriterT' edges start end)

readerWriterExample :: ReaderT Int (Writer String) Int
readerWriterExample = do
  x <- ask
  lift . tell $ show x
  return $ x + 1

--- exercise 7-6

factorialState :: StateT Integer (State Integer) ()
factorialState = do
  counter <- get
  x <- lift get
  if counter == 0
    then return ()
    else do
      put $ counter - 1
      lift . put $ x * counter
      factorialState

runFact :: Integer -> Integer
runFact x = execState (execStateT factorialState x) 1

-- exercise 7-7

-- pathsWriterT' :: [(Int, Int)] -> Int -> Int -> WriterT [Int] [] ()
-- pathsWriterT' edges start end =
--   let e_paths = do
--         (e_start, e_end) <- lift edges
--         guard $ e_start == start
--         tell [start]
--         pathsWriterT' edges e_end end
--    in if start == end
--         then tell [start] `mplus` e_paths
--         else e_paths

-- pathsMT ::
--   (MonadReader [(Int, Int)] m, MonadWriter [[Int]] m, MonadPlus m, Monad m) =>
--   Int ->
--   Int ->
--   m ()
-- pathsMT start end =
--   let e_paths = do
--         edges <- ask
--         (e_start, e_end) <- msum $ map return edges
--         guard $ e_start == start
--         tell [start]
--         pathsMT e_end end
--    in if start == end
--         then _ `mplus` e_paths
--         else e_paths

pathsWriterT2 :: (MonadReader [(Int, Int)] m, MonadWriter [Int] m, MonadPlus m) => Int -> Int -> m ()
pathsWriterT2 start end =
  let e_paths =
        ask >>= \edges ->
          (msum $ map return edges) >>= \(e_start, e_end) ->
            (guard $ e_start == start) >>= \_ ->
              tell [start] >> pathsWriterT2 e_end end
   in if start == end
        then tell [start] `mplus` e_paths
        else e_paths