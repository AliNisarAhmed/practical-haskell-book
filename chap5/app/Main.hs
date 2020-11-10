module Main where

import Lib

main :: IO ()
main = putStrLn $ show result

primes :: [Integer]
primes = sieve [2..]

sieve :: [Integer] -> [Integer]
sieve (p:xs) = p : sieve [x | x <- xs, x `rem` p /= 0]

result = foldr (*) 1 [1 .. 1000000]