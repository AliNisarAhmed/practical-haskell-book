{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Data.List
import Ranges (Range (), RangeObs (..), r, range, pattern RPattern)

main :: IO ()
main = do
  putStrLn "hello world"

sayHello :: [String] -> [String]
sayHello names =
  map
    ( \case
        "Ali" -> "Hello, Ali"
        _ -> "Welcome, stranger"
    )
    names

data Client i
  = GovOrg {clientId :: i, clientName :: String}
  | Company {clientId :: i, clientName :: String, person :: Person, duty :: String}
  | Individual {clientId :: i, person :: Person}
  deriving (Show, Eq, Ord)

data Person = Person {firstName :: String, lastName :: String}
  deriving (Eq, Ord, Show)

filterGovOrgs :: [Client a] -> [Client a]
filterGovOrgs =
  filter
    ( \case
        GovOrg {} -> True
        _ -> False
    )

getSpanOfRange :: Range -> String
getSpanOfRange rng =
  case rng of
    (r -> R a b) -> "[" ++ show a ++ ", " ++ show b ++ "]"

getSpanOfRange2 :: Range -> String
getSpanOfRange2 (RPattern a b) = "[" ++ show a ++ ", " ++ show b ++ "]"

myProd :: [Integer] -> Integer
myProd [] = 1
myProd (x : xs) = x * myProd xs

myProd2 :: [Int] -> Int
myProd2 = foldr (*) 1

myAll :: [Bool] -> Bool
myAll [] = True
myAll (x : xs) = x && myAll xs

minimumClient :: [Client a] -> Client a
minimumClient xs = minimumBy2 (\x -> getLengthOfClientName x) xs

getLengthOfClientName :: Client a -> Int
getLengthOfClientName x =
  case x of
    GovOrg {clientName} -> length clientName
    Company {clientName} -> length clientName
    Individual {person = Person {firstName, lastName}} -> length firstName + length lastName

minimumBy2 :: Ord b => (a -> b) -> [a] -> a
minimumBy2 f xs = foldr1 (\x y -> if f x < f y then x else y) xs

listOfClients =
  [ Individual 2 (Person "H. G." "Wells"),
    GovOrg 3 "NTTF", -- National Time Travel Foundation
    Company 4 "Wormhole Inc." (Person "Karl" "Schwarzschild") "Physicist",
    Individual 5 (Person "Doctor" ""),
    Individual 6 (Person "Sarah" "Jane")
  ]