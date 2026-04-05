module Homework02 where

import Data.Foldable (toList, length)

data Sequence a = Empty | Single a | Append (Sequence a) (Sequence a)
    deriving (Show, Eq)
-- 1. Functor for Sequence
instance Functor Sequence where
    fmap f Empty = Empty
    fmap f (Single x) = Single (f x)
    fmap f (Append left right) = Append (fmap f left) (fmap f right)
-- 2. Foldable for Sequence
instance Foldable Sequence where
    foldMap f Empty = mempty
    foldMap f (Single x) = f x
    foldMap f (Append left right) = foldMap f left <> foldMap f right

seqToList :: Sequence a -> [a]
seqToList = toList

seqLength :: Sequence a -> Int
seqLength = length
-- 3. Semigroup and Monoid for Sequence

instance Semigroup (Sequence a) where
    Empty <> seq = seq
    seq <> Empty = seq
    left <> right = Append left right

instance Monoid (Sequence a) where
    mempty = Empty
-- 4. Tail recursion and sequence search
tailElem :: Eq a => a -> Sequence a -> Bool
tailElem target sequence = go [sequence]
  where
    go [] = False
    go (Empty : stack) = go stack
    go (Single x : stack)
        | x == target = True
        | otherwise = go stack
    go (Append left right : stack) = go (left : right : stack)

-- 5. Tail recursion and sequence flatten
tailToList :: Sequence a -> [a]
tailToList sequence = reverse $ go [sequence] []
  where
    go [] acc = acc
    go (Empty : stack) acc = go stack acc
    go (Single x : stack) acc = go stack (x : acc)
    go (Append left right : stack) acc = go (left : right : stack) acc

-- 6. Tail recursion and Reverse Polish Notation
data Token = TNum Int | TAdd | TSub | TMul | TDiv
    deriving (Show, Eq)

tailRPN :: [Token] -> Maybe Int
tailRPN tokens = go tokens []
  where
    go [] [result] = Just result
    go [] _ = Nothing

    go (TNum n : rest) stack = go rest (n : stack)

    go (TAdd : rest) (x : y : stack) = go rest ((y + x) : stack)
    go (TSub : rest) (x : y : stack) = go rest ((y - x) : stack)
    go (TMul : rest) (x : y : stack) = go rest ((y * x) : stack)
    go (TDiv : rest) (0 : y : stack) = Nothing
    go (TDiv : rest) (x : y : stack) = go rest ((y `div` x) : stack)

    go (_ : _) _ = Nothing

-- 7. Expressing functions via foldr / foldl
-- (a)
myReverse :: [a] -> [a]
myReverse = foldl (\acc x -> x : acc) []
-- (b)
myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile predicate = foldr(\x acc ->if predicate x then x : acc else [])[]
-- (c)
decimal :: [Int] -> Int
decimal = foldl (\acc digit -> acc * 10 + digit) 0
-- 8. Run-length encoding via folds
encode :: Eq a => [a] -> [(a, Int)]
encode =
    foldr step []
  where
    step x [] = [(x, 1)]
    step x ((y, n) : ys)
        | x == y = (y, n + 1) : ys
        | otherwise = (x, 1) : (y, n) : ys
decode :: [(a, Int)] -> [a]
decode = foldr (\(x, n) acc -> replicate n x ++ acc) []