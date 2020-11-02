{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Lib where

import Data.Graph
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Tree (Tree (Node))

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- M.insert

insert :: Ord k => k -> a -> M.Map k a -> M.Map k a
insert k v map =
  M.alter (\_ -> Just v) k map

delete :: Ord k => k -> M.Map k a -> M.Map k a
delete = M.alter (const Nothing)

adjust :: Ord k => (a -> a) -> k -> M.Map k a -> M.Map k a
adjust f = M.alter (fmap f)

m = M.fromList [("hello", 3), ("bye", 4)]

-- Exercise 4-3

data Client i
  = GovOrg {clientId :: i, clientName :: String}
  | Company {clientId :: i, clientName :: String, person :: Person, duty :: String}
  | Individual {clientId :: i, person :: Person}
  deriving (Show)

data Person = Person {firstName :: String, lastName :: String}
  deriving (Show)

data ClientKind = GovOrgKind | CompanyKind | IndividualKind deriving (Eq, Ord, Show)

listOfClients :: [Client Integer]
listOfClients =
  [ Individual 2 (Person "H. G." "Wells"),
    GovOrg 3 "NTTF", -- National Time Travel Foundation
    Company 4 "Wormhole Inc." (Person "Karl" "Schwarzschild") "Physicist",
    Individual 5 (Person "Doctor" "Who"),
    Individual 6 (Person "Sarah" "Jane")
  ]

empty :: M.Map ClientKind (S.Set (Client Integer))
empty = M.fromList [(GovOrgKind, S.empty), (CompanyKind, S.empty), (IndividualKind, S.empty)]

classifyClients :: [Client Integer] -> M.Map ClientKind (S.Set (Client Integer))
classifyClients = foldr step empty
  where
    step (g@GovOrg {}) acc = M.adjust (S.insert g) GovOrgKind acc
    step (c@Company {}) acc = M.adjust (S.insert c) CompanyKind acc
    step (i@Individual {}) acc = M.adjust (S.insert i) IndividualKind acc

classifyClients2 :: [Client Integer] -> M.Map ClientKind (S.Set (Client Integer))
classifyClients2 =
  M.fromList
    . fmap (\s -> (classify $ head s, S.fromList s))
    . L.groupBy (\a b -> classify a == classify b)

classify :: Client Integer -> ClientKind
classify (GovOrg {}) = GovOrgKind
classify (Company {}) = CompanyKind
classify (Individual {}) = IndividualKind

---- Trees

preorder :: (a -> b) -> Tree a -> [b]
preorder f (Node v subtrees) =
  let subtreesTraversed = concatMap (preorder f) subtrees
   in f v : subtreesTraversed

pictureTree :: Tree Int
pictureTree =
  Node
    1
    [ Node
        2
        [ Node 3 [],
          Node 4 [],
          Node 5 []
        ],
      Node 6 []
    ]

---- Graphs

timeMachineGraph :: [(String, String, [String])]
timeMachineGraph =
  [ ("wood", "wood", ["walls"]),
    ("plastic", "plastic", ["walls", "wheels"]),
    ("aluminum", "aluminum", ["wheels", "door"]),
    ("walls", "walls", ["done"]),
    ("wheels", "wheels", ["done"]),
    ("door", "door", ["done"]),
    ("done", "done", [])
  ]

timeMachinePrecedence ::
  (Graph, Vertex -> (String, String, [String]), String -> Maybe Vertex)
timeMachinePrecedence = graphFromEdges timeMachineGraph

timeMachineTravel :: Graph
timeMachineTravel =
  buildG
    (103, 2013)
    [ (1302, 1614),
      (1614, 1302),
      (1302, 2013),
      (2013, 1302),
      (1614, 2013),
      (2013, 1408),
      (1408, 1993),
      (1408, 917),
      (1993, 917),
      (917, 103),
      (103, 917)
    ]

---- TypeClasses

class Nameable n where
  name :: n -> String

instance Nameable (Client i) where
  name Individual {person = Person {firstName, lastName}} = firstName ++ " " ++ lastName
  name c = clientName c

-- Exercise 4-4

data TimeMachine = TimeMachine
  { manufacturer :: Manufacturer,
    _name :: Name,
    direction :: Direction,
    _price :: Price
  }
  deriving (Show)

newtype Manufacturer = Manufacturer String deriving (Show)

newtype Model = Model Int deriving (Show)

newtype Name = Name String deriving (Show)

data Direction = PAST | FUTURE deriving (Show)

data Price = Price {value :: Double} deriving (Show)

class Priceable a where
  price :: a -> Double

instance Priceable TimeMachine where
  price (TimeMachine {_price = Price {value}}) = value

totalPrice :: Priceable p => [p] -> Double
totalPrice = foldr (\x acc -> price x + acc) 0.0

-- Exercise 4-5

instance Eq Person where
  (==)
    (Person {firstName = f1, lastName = l1})
    (Person {firstName = f2, lastName = l2}) = f1 == f2 && l1 == l2

instance Eq a => Eq (Client a) where
  (==) (GovOrg {clientId = c1, clientName = n1}) (GovOrg {clientId = c2, clientName = n2}) =
    c1 == c2 && n1 == n2
  (==)
    (Company {clientId = c1, clientName = n1, person = p1, duty = d1})
    (Company {clientId = c2, clientName = n2, person = p2, duty = d2}) =
      c1 == c2 && n1 == n2 && p1 == p2 && d1 == d2
  (==)
    (Individual {clientId = c1, person = p1})
    (Individual {clientId = c2, person = p2}) =
      c1 == c2 && p1 == p2
  (==) _ _ = False

instance Ord Person where
  compare (Person f1 l1) (Person f2 l2) =
    case compare f1 f2 of
      LT -> LT
      GT -> GT
      EQ -> compare l1 l2

instance (Eq i) => Ord (Client i) where
  compare c1 c2 =
    case compare (name c1) (name c2) of
      LT -> LT
      GT -> GT
      EQ ->
        case (c1, c2) of
          (Individual _ p1, Individual _ p2) -> compare p1 p2
          (Individual {}, _) -> GT
          (_, Individual {}) -> LT
          (Company _ _ p1 _, Company _ _ p2 _) -> compare p1 p2
          (Company {}, _) -> GT
          (_, Company {}) -> LT
          (GovOrg _ n1, GovOrg _ n2) -> compare n1 n2

-- Binary Trees for Minimum Price

data TravelGuide = TravelGuide {title :: String, authors :: [String], tprice :: Double}
  deriving (Eq, Show)

data BinaryTree1
  = Node1 TravelGuide BinaryTree1 BinaryTree1
  | Leaf1
  deriving (Eq, Ord, Show)

treeFind1 :: TravelGuide -> BinaryTree1 -> Maybe TravelGuide
treeFind1 t (Node1 v l r) =
  case compare t v of
    EQ -> Just v
    LT -> treeFind1 t l
    GT -> treeFind1 t r
treeFind1 t (Leaf1) = Nothing

treeInsert1 :: TravelGuide -> BinaryTree1 -> BinaryTree1
treeInsert1 t n@(Node1 v l r) =
  case compare t v of
    EQ -> n
    LT -> treeInsert1 t l
    GT -> treeInsert1 t r
treeInsert1 t Leaf1 = Node1 t Leaf1 Leaf1

data BinaryTree2 a
  = Node2 a (BinaryTree2 a) (BinaryTree2 a)
  | Leaf2
  deriving (Show)

treeFind2 :: Ord a => a -> BinaryTree2 a -> Maybe a
treeFind2 t (Node2 v l r) =
  case compare t v of
    EQ -> Just v
    LT -> treeFind2 t l
    GT -> treeFind2 t r
treeFind2 _ Leaf2 = Nothing

treeInsert2 :: Ord a => a -> BinaryTree2 a -> BinaryTree2 a
treeInsert2 t n@(Node2 v l r) =
  case compare t v of
    EQ -> n
    LT -> treeInsert2 t l
    GT -> treeInsert2 t r
treeInsert2 t (Leaf2) = Node2 t Leaf2 Leaf2

treeConcat :: Ord a => BinaryTree2 a -> BinaryTree2 a -> BinaryTree2 a
treeConcat Leaf2 t = t
treeConcat t Leaf2 = t
treeConcat t (Node2 v l r) = treeConcat r $ treeConcat l (treeInsert2 v t)

instance Ord TravelGuide where
  (<=) (TravelGuide t1 a1 p1) (TravelGuide t2 a2 p2) =
    p1 < p2 || (p1 == p2 && (t1 < t2 || (t1 == t2 && a1 <= a2)))

-- Binary Tree with cache for smallest element

data BinaryTree3 v c
  = Node3 v c (BinaryTree3 v c) (BinaryTree3 v c)
  | Leaf3
  deriving (Eq, Ord, Show)

treeInsert3 :: (Ord v, Ord c) => v -> c -> BinaryTree3 v c -> BinaryTree3 v c
treeInsert3 v c (Node3 v2 c2 l r) =
  case compare v v2 of
    EQ -> Node3 v2 c2 l r
    LT -> Node3 v2 (min c c2) (treeInsert3 v c l) r
    GT -> Node3 v2 (min c c2) l (treeInsert3 v c r)

---- Using Monoid for cache

treeInsert4 :: (Ord v, Monoid c) => v -> c -> BinaryTree3 v c -> BinaryTree3 v c
treeInsert4 v c (Node3 v2 c2 l r) =
  case compare v v2 of
    EQ -> Node3 v2 c2 l r
    LT ->
      let newLeft = treeInsert4 v c l
          newCache = c2 <> getCache newLeft <> getCache r
       in Node3 v2 newCache newLeft r
    GT ->
      let newRight = treeInsert4 v c r
          newCache = c2 <> getCache l <> getCache newRight
       in Node3 v2 newCache l newRight
treeInsert4 v c Leaf3 = Node3 v c Leaf3 Leaf3

getCache :: Monoid c => BinaryTree3 v c -> c
getCache (Node3 _ c _ _) = c
getCache Leaf3 = mempty

--

instance Functor (BinaryTree2) where
  fmap f Leaf2 = Leaf2
  fmap f (Node2 v l r) = Node2 (f v) (fmap f l) (fmap f r)