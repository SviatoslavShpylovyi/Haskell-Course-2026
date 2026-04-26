{-# LANGUAGE DeriveFunctor #-}

module Homework03 where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Control.Monad
import Data.List (permutations)
import Data.Monoid
-- 1. Maybe Monad: Maze navigation

type Pos = (Int,Int)
data Dir = N | S | E | W
    deriving (Eq,Ord,Show)
type Maze = Map Pos (Map Dir Pos)
move:: Maze ->Pos ->Dir ->Maybe Pos
move maze pos dir = do
    neighbours<-Map.lookup pos maze
    Map.lookup dir neighbours
followPath :: Maze -> Pos -> [Dir] -> Maybe Pos
followPath maze pos [] = return pos
followPath maze pos (d:ds) = do
  next <- move maze pos d
  followPath maze next ds
safePath :: Maze -> Pos -> [Dir] -> Maybe [Pos]
safePath maze pos [] = return [pos]
safePath maze pos (d:ds) = do
  next <- move maze pos d
  rest <- safePath maze next ds
  return (pos : rest)

-- 2. Maybe Monad: Decryption

type Key = Map Char Char

decrypt :: Key -> String -> Maybe String
decrypt key text =
  traverse (`Map.lookup` key) text

decryptWords :: Key -> [String] -> Maybe [String]
decryptWords key wordsList =
  traverse (decrypt key) wordsList

-- 3. List Monad: Seating arrangements

type Guest = String
type Conflict = (Guest, Guest)
seatings:: [Guest]->[Conflict]->[[Guest]]
seatings guests conflicts = do
    seating <- permutations guests
    guard  (validSeating seating conflicts)
    return seating

validSeating :: [Guest]->[Conflict]->Bool
validSeating [] _ = True
validSeating [_] _ = True
validSeating guests conflicts = 
    all(not . hasConflicts conflicts) pairs
    where
        pairs = zip guests (drop 1 guests) ++ [(lastSafe guests, firstSafe guests)]

firstSafe :: [a] ->a
firstSafe(x:_) = x
lastSafe ::[a] ->a
lastSafe [x] =x 
lastSafe(_:xs) = lastSafe xs





hasConflicts :: [Conflict]->(Guest,Guest) ->Bool
hasConflicts conflicts(a,b) = (a,b) `elem` conflicts || (b,a) `elem` conflicts

-- 4. Custom Monad: Result with warnings
data Result a
  = Failure String
  | Success a [String]
  deriving Show

instance Functor Result where
  fmap f (Success x warnings) =
    Success (f x) warnings
  fmap _ (Failure message) =
    Failure message
instance Applicative Result where 
    pure x = Success x []
    Success f warnings1 <*> Success x warnings2 = Success(f x) (warnings1++warnings2 )
    Failure message <*> _ = Failure message
    _ <*> Failure message = Failure message
instance Monad Result where
    Success x warning1 >>= f = 
        case f x of 
            Success y warning2 ->
                Success y (warning1 ++ warning2)

            Failure message -> Failure message
    Failure message >>= _ = Failure message
warn :: String -> Result()
warn message = Success()[message]
failure :: String-> Result a
failure message = Failure message

validateAge :: Int -> Result Int
validateAge age 
    | age < 0 = failure "Age cannot be negative"
    | age > 150 = do 
        warn "Age is unusually high"
        return age
    | otherwise = return age

validateAges:: [Int]->Result [Int]
validateAges ages = mapM validateAge ages


-- 5. Custom Writer Monad: Simplification log

newtype Writer m a = Writer { runWriter :: (a, m) }
  deriving (Show, Functor)

instance Monoid m => Applicative (Writer m) where
  pure x =
    Writer (x, mempty)

  liftA2 f (Writer (x, logx)) (Writer (y, logy)) =
    Writer (f x y, logx <> logy)

instance Monoid m => Monad (Writer m) where
  Writer (x, logx) >>= f =
    let (y, logy) = runWriter (f x)
    in Writer (y, logx <> logy)

tell :: m -> Writer m ()
tell message =
  Writer ((), message)

data Expr
  = Lit Int
  | Add Expr Expr
  | Mul Expr Expr
  | Neg Expr
  deriving (Eq, Show)

simplify :: Expr -> Writer [String] Expr
simplify (Lit n) =
  return (Lit n)

simplify (Add left right) = do
  left' <- simplify left
  right' <- simplify right

  case (left', right') of
    (Lit 0, e) -> do
      tell ["Add identity: 0 + e -> e"]
      return e

    (e, Lit 0) -> do
      tell ["Add identity: e + 0 -> e"]
      return e

    (Lit a, Lit b) -> do
      tell ["Constant folding: a + b"]
      return (Lit (a + b))

    _ ->
      return (Add left' right')

simplify (Mul left right) = do
  left' <- simplify left
  right' <- simplify right

  case (left', right') of
    (Lit 0, _) -> do
      tell ["Zero absorption: 0 * e -> 0"]
      return (Lit 0)

    (_, Lit 0) -> do
      tell ["Zero absorption: e * 0 -> 0"]
      return (Lit 0)

    (Lit 1, e) -> do
      tell ["Multiplicative identity: 1 * e -> e"]
      return e

    (e, Lit 1) -> do
      tell ["Multiplicative identity: e * 1 -> e"]
      return e

    (Lit a, Lit b) -> do
      tell ["Constant folding: a * b"]
      return (Lit (a * b))

    _ ->
      return (Mul left' right')

simplify (Neg expr) = do
  expr' <- simplify expr

  case expr' of
    Neg inner -> do
      tell ["Double negation: -(-e) -> e"]
      return inner

    _ ->
      return (Neg expr')
-- 6. ZipList

newtype ZipList a = ZipList {getZipList :: [a]} deriving Show
instance Functor ZipList where
  fmap f (ZipList xs) =
    ZipList (map f xs)

instance Applicative ZipList where
  pure x =
    ZipList (repeat x)

  ZipList fs <*> ZipList xs =
    ZipList (zipWith ($) fs xs)



-- ZipList cannot have a lawful Monad instance.
-- In Applicative, ZipList combines elements position by position.
-- For Monad, (>>=) would have type:
--   ZipList a -> (a -> ZipList b) -> ZipList b
-- But the function may return ZipLists of different lengths for different elements.
-- There is no lawful way to choose how to combine these lists positionally
-- while also satisfying the monad laws.
-- Therefore ZipList is Applicative, but not a lawful Monad.

