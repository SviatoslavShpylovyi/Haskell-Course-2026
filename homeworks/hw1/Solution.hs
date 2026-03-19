{-# LANGUAGE BangPatterns #-}
module Solution where
import Data.Function
import Data.List(nub)
--isPrime I used it before doing Exercise_3
-- isPrime :: Int -> Bool
-- isPrime n 
--  | n <= 1 = False
--  | n == 2 = True
--  | even n = False
--  | otherwise = all (/= 0) [ n `mod` i | i <- [2..s]]
--   where 
--     s = n & fromIntegral & sqrt & floor 

--Goldbach Pairs
goldbachPairs :: Int->[(Int,Int)]
goldbachPairs n
    | n < 4 = []
    | odd n = []
    | otherwise = [(p,q) | p<-[2..n], let q = n -p, p<=q, isPrime p, isPrime q]
--Coprime Pairs
coprimePairs :: [Int]->[(Int,Int)]
coprimePairs [] = []
coprimePairs (xs) = [(x,y) | x<-nub xs, y<-nub xs, x<y, gcd x y ==1] 
-- Sieve of Eratosthene
sieve :: [Int] ->[Int]
sieve [] = []
sieve(p:xs) = p : sieve[x | x<-xs, mod x p /=0]

primesTo :: Int ->[Int]
primesTo n = sieve [2..n]

isPrime :: Int->Bool
isPrime n
    | n<2 = False
    | otherwise = elem n (primesTo n)
--MatrixMul check for the requirements once again
matMul :: [[Int]] -> [[Int]] -> [[Int]]
matMul [] [] = []
matMul _ [] = []
matMul [] _ = []
matMul a b =
    [ [ sum [ a !! i !! k * b !! k !! j | k <- [0 .. p - 1] ]
      | j <- [0 .. n - 1] ]
    | i <- [0 .. m - 1] ]
  where
    m = length a
    p = length (head a)
    n = length (head b)
-- Permutations
permutations :: Int -> [a] -> [[a]]
permutations 0 _  = [[]]
permutations _ [] = []
permutations k xs =
    [ y:ys | (y, rest) <- choose xs, ys <- permutations (k-1) rest ]

choose :: [a] -> [(a, [a])]
choose [] = []
choose (x:xs) = (x, xs) : [ (y, x:ys) | (y, ys) <- choose xs ]
--Section 2
--Hamming Nums
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = nub xs
merge [] ys = nub ys
merge (x:xs) (y:ys) 
    | x<y = x:merge xs (y:ys)
    | y<x = y:merge (x:xs) ys
    | otherwise = x:merge xs ys
hamming :: [Integer]
hamming = 1 : merge(map(2*) hamming) (merge(map(3*) hamming) (map (5*) hamming))
--Power
power :: Int -> Int -> Int
power _ 0 = 1
power 0 _ = 0
power a b = go a b 1
    where 
     go a 0 acc = acc
     go a !b !acc = go a (b-1) (a*acc) 
-- list Maximum
listMaxBang :: [Int]->Int
listMaxBang [] = 0
listMaxBang (x:xs) = go xs x 
    where
     go [] acc = acc
     go (x:xs) !acc = go xs (if x>=acc then x else acc)
listMaxSeq :: [Int]->Int
listMaxSeq [] = 0
listMaxSeq (x:xs) = go xs x
    where 
     go [] acc = acc
     go (x:xs) acc = 
        let newAcc = if x >= acc then x else acc
        in newAcc `seq` go xs newAcc
-- Infinite Prime 
primes :: [Int]
primes  = sieve[2..]

isPrime_2 :: Int -> Bool
isPrime_2 n
    | n<2 = False
    | otherwise = elem n (takeWhile (<=n) primes)
-- Mean
mean_a :: [Double] -> Double
mean_a [] = 0
mean_a xs = s/n
    where 
        (s,n) = go xs (0.0, 0.0)
        go [] acc = acc
        go (x:xs)(s,n) = go xs (s+x, n+1.0)

mean_b :: [Double] -> Double
mean_b [] = 0
mean_b xs = s / n
  where
    (s, n) = go xs 0.0 0.0

    go [] !s !n = (s, n)
    go (x:xs) !s !n = go xs (s + x) (n + 1.0)

meanVar :: [Double] -> (Double, Double)
meanVar [] = (0,0)
meanVar xs = (mu, var)
  where
    (n, s, ss) = go xs 0.0 0.0 0.0

    mu  = s / n
    var = ss / n - mu * mu

    go [] !n !s !ss = (n, s, ss)
    go (x:xs) !n !s !ss = go xs (n + 1.0) (s + x) (ss + x * x)