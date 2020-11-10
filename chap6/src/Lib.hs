{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib where

import Control.Monad.ST
import Control.Monad.State
import Control.Monad.Writer
import Data.Char (toUpper)
import Data.List
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import Data.STRef
import Lens.Micro.Platform

someFunc :: IO ()
someFunc = putStrLn "someFunc"

class Ord v => Vector v where
  distance :: v -> v -> Double
  centroid :: [v] -> v

instance Vector (Double, Double) where
  distance (a, b) (c, d) = sqrt $ (a - c) ^ 2 + (b - d) ^ 2
  centroid lst =
    let (u, v) = foldr sumCoordinates (0, 0) lst
        n = fromIntegral $ length lst
     in (u / n, v / n)
    where
      sumCoordinates (a, b) (c, d) = (a + c, b + d)

class Vector v => Vectorizable e v where
  toVector :: e -> v

instance Vectorizable (Double, Double) (Double, Double) where
  toVector = id

-- this function distrbutes the "points" to each centroids so that distance between the point and the centroid is minimum
clusterAssignmentPhase ::
  (Ord v, Vector v, Vectorizable e v) =>
  [v] ->
  [e] ->
  M.Map v [e]
clusterAssignmentPhase centroids points =
  let initialMap = M.fromList $ zip centroids (repeat [])
   in foldr
        ( \p m ->
            let chosenC = minimumBy (compareDistance p) centroids
             in M.adjust (p :) chosenC m
        )
        initialMap
        points
  where
    compareDistance p x y = compare (distance x $ toVector p) (distance y $ toVector p)

-- after distribution above, this function then finds the new centroids for each [e] in M.Map v [e]
newCentroidPhase :: (Vector v, Vectorizable e v) => M.Map v [e] -> [(v, v)]
newCentroidPhase = M.toList . fmap (centroid . map toVector)

shouldStop :: (Vector v) => [(v, v)] -> Double -> Bool
shouldStop centroids threshold =
  foldr sumDistance 0.0 centroids < threshold
  where
    sumDistance (x, y) sum = distance x y + sum

kMeans ::
  (Vector v, Vectorizable e v) =>
  (Int -> [e] -> [v]) -> -- initialization function
  Int -> -- number of centroids
  [e] -> -- the information
  Double -> -- threshold
  ([v], Int) -- final centroids (with number of times the recursion happened)
kMeans initFunction k points threshold = kMeans' (initFunction k points) points threshold 0

kMeans' :: (Vector v, Vectorizable e v) => [v] -> [e] -> Double -> Int -> ([v], Int)
kMeans' centroids points threshold count =
  let assignments = clusterAssignmentPhase centroids points
      oldNewCentroids = newCentroidPhase assignments
      newCentroids = map snd oldNewCentroids
   in if shouldStop oldNewCentroids threshold
        then (newCentroids, count)
        else kMeans' newCentroids points threshold (count + 1)

-- create the initial set of centroids
initializeSimple :: Int -> [e] -> [(Double, Double)]
initializeSimple 0 _ = []
initializeSimple n v = (fromIntegral n, fromIntegral n) : initializeSimple (n - 1) v

info :: [(Double, Double)]
info = [(1, 1), (1, 2), (4, 4), (4, 5)] :: [(Double, Double)]

---- Lenses

-- data Client i
--   = GovOrg i String
--   | Company i String Person String
--   | Individual i Person

-- data Person = Person String String

-- firstName :: Lens' Person String
-- firstName = lens (\(Person f _) -> f) (\(Person _ l) newFirstName -> Person newFirstName l)

-- lastName :: Lens' Person String
-- lastName = lens (\(Person _ l) -> l) (\(Person f _) newLastName -> Person f newLastName)

-- identifier :: Lens (Client i) (Client j) i j
-- identifier =
--   lens
--     ( \case
--         (GovOrg i _) -> i
--         (Company i _ _ _) -> i
--         (Individual i _) -> i
--     )
--     ( \client newId ->
--         case client of
--           GovOrg _ n -> GovOrg newId n
--           Company _ n p r -> Company newId n p r
--           Individual _ p -> Individual newId p
--     )

fullName :: Lens' Person String
fullName =
  lens
    (\(Person f l) -> f ++ " " ++ l)
    ( \_ newFullName ->
        case words newFullName of
          f : l : _ -> Person f l
          _ -> error "Incorrect Name"
    )

-- Generating Lenses using Template Haskell

data Client i
  = GovOrg {_identifier :: i, _name :: String}
  | Company {_identifier :: i, _name :: String, _person :: Person, _duty :: String}
  | Individual {_identifier :: i, _person :: Person}
  deriving (Show)

data Person = Person {_firstName :: String, _lastName :: String}
  deriving (Show)

makeLenses ''Client
makeLenses ''Person

p = Person "Ali" "Ahmed"

client = Individual 3 p

-- Lenses for TimeMachine

data TimeMachine = TimeMachine
  { _manufacturer :: Manufacturer,
    _tname :: Name,
    _direction :: Direction,
    _price :: Price
  }
  deriving (Show)

newtype Manufacturer = Manufacturer String deriving (Show)

newtype Model = Model Int deriving (Show)

newtype Name = Name String deriving (Show)

data Direction = PAST | FUTURE deriving (Show)

data Price = Price {_value :: Double} deriving (Show)

makeLenses ''TimeMachine
makeLenses ''Price

t1 = TimeMachine (Manufacturer "VolksWagon") (Name "TimeMachine Tiguan") FUTURE (Price 20.25)

t2 = TimeMachine (Manufacturer "Toyota") (Name "TimeMachine Lexus") FUTURE (Price 42.5)

t3 = TimeMachine (Manufacturer "Honda") (Name "TimeMachine CRV") FUTURE (Price 32.5)

t4 = TimeMachine (Manufacturer "Mazda") (Name "TimeMachine CX5") FUTURE (Price 26.3)

increasePrices :: Double -> [TimeMachine] -> [TimeMachine]
increasePrices j xs = xs & traversed . price . value %~ (\p -> p + p * j)

listOfTimeMachines = [t1, t2, t3, t4]

---- Using Lenses to re-write K-Means

data KMeansState e v = KMeansState {_centroids :: [v], _points :: [e], _err :: Double, _threshold :: Double, _steps :: Int}
  deriving (Eq, Ord)

makeLenses ''KMeansState

initializeState :: (Int -> [e] -> [v]) -> Int -> [e] -> Double -> KMeansState e v
initializeState initFunc k pts t = KMeansState (initFunc k pts) pts (1.0 / 0.0) t 0

clusterAssignmentPhase2 ::
  (Ord v, Vector v, Vectorizable e v) =>
  KMeansState e v ->
  M.Map v [e]
clusterAssignmentPhase2 state =
  let initialMap = M.fromList $ zip cs (repeat [])
   in foldr
        ( \p m ->
            let chosenC = minimumBy (compareDistance p) cs
             in M.adjust (p :) chosenC m
        )
        initialMap
        ps
  where
    ps = view points state
    cs = view centroids state
    compareDistance p x y = compare (distance x $ toVector p) (distance y $ toVector p)

kMeans2 ::
  (Vector v, Vectorizable e v) =>
  (Int -> [e] -> [v]) ->
  Int ->
  [e] ->
  Double ->
  [v]
kMeans2 initFunc k pts t = view centroids $ kMeans'' (initializeState initFunc k pts t)

kMeans'' ::
  (Vector v, Vectorizable e v) =>
  KMeansState e v ->
  KMeansState e v
kMeans'' state =
  let assignments = clusterAssignmentPhase2 state
      state1 = state & centroids . traversed %~ (\c -> centroid $ fmap toVector $ M.findWithDefault [] c assignments)
      state2 = state1 & err .~ sum (zipWith distance (state ^. centroids) (state1 ^. centroids))
      state3 = state2 & steps +~ 1
   in if state3 ^. err < state3 ^. threshold
        then state3
        else kMeans'' state3

-- Missing Values

-- type State s a = s -> (a, s)

-- thenDo :: State s a -> (a -> State s b) -> State s b
-- thenDo sa f = \s ->
--   let (result, nextState) = sa s
--    in f result nextState

data KMeansState2 v = KMeansState2 {centroids2 :: [v], threshold2 :: Double, steps2 :: Int}

newCentroids2 :: (Vector v, Vectorizable e v) => M.Map v [e] -> [v]
newCentroids2 = M.elems . fmap (centroid . map toVector)

clusterAssignments2 :: (Vector v, Vectorizable e v) => [v] -> [e] -> M.Map v [e]
clusterAssignments2 centers points =
  let initialMap = M.fromList $ zip centers (repeat [])
   in foldr
        ( \p m ->
            let chosenC = minimumBy (compareDistance p) centers
             in M.adjust (p :) chosenC m
        )
        initialMap
        points
  where
    compareDistance p x y = compare (distance x $ toVector p) (distance y $ toVector p)

-- kMeans3 :: (Vector v, Vectorizable e v) => [e] -> State (KMeansState2 v) [v]
-- kMeans3 points =
--   (\s -> (centroids s, s))
--     `thenDo` ( \prevCenters ->
--                  (\s -> (clusterAssignments2 prevCenters points, s))
--                    `thenDo` ( \assignments ->
--                                 (\s -> (newCentroids2 assignments, s))
--                                   `thenDo` ( \newCenters ->
--                                                (\s -> ((), s {centroids2 = newCenters}))
--                                                  `thenDo` ( \_ ->
--                                                               (\s -> ((), s {steps2 = steps2 s + 1}))
--                                                                 `thenDo` ( \_ ->
--                                                                              (\s -> (theshold2 s, s))
--                                                                                `thenDo` ( \t ->
--                                                                                             (\s -> (sum $ zipWith distance prevCenters newCenters, s))
--                                                                                               `thenDo` ( \err ->
--                                                                                                            if err < t then (\s -> (newCenters, s)) else (kMeans3 points)
--                                                                                                        )
--                                                                                         )
--                                                                          )
--                                                           )
--                                            )
--                             )
--              )

-- remain :: a -> (s -> (a, s))
-- remain x = \s -> (x, s)

-- access :: (s -> a) -> (s -> (a, s))
-- access f = \s -> (f s, s)

-- modify :: (s -> s) -> (s -> ((), s))
-- modify f = (\s -> ((), f s))

-- kMeans4 :: (Vector v, Vectorizable e v) => [e] -> State (KMeansState2 v) [v]
-- kMeans4 points =
--   access centroids
--     `thenDo` ( \prevCenters ->
--                  remain (clusterAssignments2 prevCenters points)
--                    `thenDo` ( \assignments ->
--                                 modify (\s -> s {centroids2 = newCenters})
--                                   `thenDo` ( \_ ->
--                                                modify (\s -> s {steps2 = steps2 s + 1})
--                                                  `thenDo` ( \_ ->
--                                                               access threshold2
--                                                                 `thenDo` ( \t ->
--                                                                              remain (sum $ zipWith distance prevCenters newCenters)
--                                                                                `thenDo` ( \err ->
--                                                                                             if err < t then remain newCenters else kMeans4 points
--                                                                                         )
--                                                                          )
--                                                           )
--                                            )
--                             )
--              )

kMeans5 :: (Vector v, Vectorizable e v) => [e] -> State (KMeansState2 v) [v]
kMeans5 points = do
  prevCenters <- gets centroids2
  let assignments = clusterAssignmentPhase prevCenters points
      newCenters = newCentroids2 assignments
  modify (\s -> s {centroids2 = newCenters})
  modify (\s -> s {steps2 = steps2 s + 1})
  t <- fmap threshold2 get
  let err = sum $ zipWith distance prevCenters newCenters
  if err < t
    then return newCenters
    else kMeans5 points

data ExampleSt = ExampleSt {_increment :: Int, _clients :: [Client Int]}
  deriving (Show)

makeLenses ''ExampleSt

zoomCl :: State ExampleSt ()
zoomCl = do
  n <- use increment
  zoom (clients . traversed) $ do
    identifier += n
    person . fullName %= map toUpper

client1 = Individual 4 (Person "John" "Smith")

client2 = Individual 3 (Person "Albert" "Einstein")

-- Writer

newtype MyWriter m a = MyWriter {runWriter2 :: (a, m)}

instance Functor (MyWriter m) where
  fmap f (MyWriter (a, m)) = MyWriter (f a, m)

instance Monoid m => Applicative (MyWriter m) where
  pure x = MyWriter (x, mempty)
  (MyWriter (f, m1)) <*> (MyWriter (a, m2)) =
    MyWriter (f a, m1 <> m2)

instance Monoid m => Monad (MyWriter m) where
  (MyWriter (a, m1)) >>= f =
    let (b, m2) = runWriter2 $ f a
     in MyWriter (b, m1 <> m2)

tell2 :: Monoid m => m -> MyWriter m ()
tell2 x = MyWriter ((), x)

---- using ST, a monad for mutation

listLength :: [a] -> Integer
listLength list = runST $ do
  i <- newSTRef 0
  traverseList list i
  readSTRef i
  where
    traverseList [] _ = return ()
    traverseList (_ : xs) l = do
      modifySTRef' l (+ 1)
      traverseList xs l

-- kMeans' :: (Vector v, Vectorizable e v) => [v] -> [e] -> Double -> Int -> ([v], Int)
-- kMeans' centroids points threshold count =
--   let assignments = clusterAssignmentPhase centroids points
--       oldNewCentroids = newCentroidPhase assignments
--       newCentroids = map snd oldNewCentroids
--    in if shouldStop oldNewCentroids threshold
--         then (newCentroids, count)
--         else kMeans' newCentroids points threshold (count + 1)

-- exercise 6-7

kMeans7 :: (Vector v, Vectorizable e v) => STRef s [v] -> [e] -> Double -> STRef s Int -> ST s ([v], Int)
kMeans7 centroids points threshold iteration = do
  prevCenters <- readSTRef centroids
  iter <- readSTRef iteration
  let assignments = clusterAssignmentPhase prevCenters points
      oldNewCentroids = newCentroids2 assignments
      err = sum $ zipWith distance prevCenters oldNewCentroids
  writeSTRef centroids oldNewCentroids
  modifySTRef' iteration (+ 1)
  if err < threshold
    then return (oldNewCentroids, iter)
    else kMeans7 centroids points threshold iteration