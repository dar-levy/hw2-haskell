{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -Werror #-}
-- Refines the above, allowing for unused imports.
{-# OPTIONS_GHC -Wno-unused-imports #-}

module HW2 where

import Data.List (find, foldl')
import Prelude (Bool (..), Bounded (..), Char, Either (..), Enum (..), Eq (..), Int, Integer, Maybe (..), Num (..), Ord (..), Show (..), String, all, and, any, concat, concatMap, const, curry, div, elem, error, even, filter, flip, foldl, foldr, fst, id, length, lines, lookup, map, mod, not, notElem, null, odd, otherwise, product, snd, sum, uncurry, undefined, unlines, unwords, words, (!!), ($), (&&), (++), (.), (||))

------------------------------------------------
-- DO NOT MODIFY ANYTHING ABOVE THIS LINE !!! --
------------------------------------------------

-- Section 1.1: Basic Maybes
concatMaybeMap :: (a -> Maybe b) -> Maybe a -> Maybe b
concatMaybeMap _ Nothing = Nothing
concatMaybeMap f (Just a) = f a


fromMaybe :: a -> Maybe a -> a
fromMaybe x Nothing = x
fromMaybe _ (Just a) = a


maybe :: b -> (a -> b) -> Maybe a -> b
maybe b _ Nothing = b
maybe _ f (Just a) = f a

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Nothing : xs) = catMaybes xs
catMaybes (Just x : xs) = x : catMaybes xs

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe _ [] = []
mapMaybe f (x : xs) = case (f x) of
  Nothing -> mapMaybe f xs
  Just y -> y : mapMaybe f xs


-- Section 1.2 Basic Eithers
concatEitherMap :: (a -> Either e b) -> Either e a -> Either e b
concatEitherMap _ (Left e) = Left e
concatEitherMap f (Right a) = f a

either :: (a -> c) -> (b -> c) -> Either a b -> c
either f _ (Left a) = f a
either _ g (Right b) = g b

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f (Left a) = Left (f a)
mapLeft _ (Right b) = Right b 


catEithers :: [Either e a] -> Either e [a]
catEithers (Left e : _) = Left e
catEithers [] = Right []
catEithers (Right x : xs) = case catEithers xs of
  Left e -> Left e
  Right seq -> Right (x : seq)


mapEither :: (a -> Either e b) -> [a] -> Either e [b]
mapEither _ [] = Right []
mapEither f (x : xs) = case f x of
  Left e -> Left e
  Right y -> case mapEither f xs of
    Left e -> Left e
    Right seq -> Right (y : seq)


partitionEithers :: [Either a b] -> ([a], [b])
partitionEithers seq = (leftList seq, rightList seq) where
  leftList :: [Either a b] -> [a]
  leftList [] = []
  leftList (Left a : xs) = a : (leftList xs)
  leftList (Right _ : xs) = leftList xs
  rightList :: [Either a b] -> [b]
  rightList [] = []
  rightList (Right b : xs) = b : (rightList xs)
  rightList (Left _ : xs) = rightList xs


eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right b) = Just b

-- Section 2: Lists
take :: Int -> [a] -> [a]
take n xs =
  if n <= 0 then []
  else case xs of
    [] -> []
    (x : xs) -> x : take (n - 1) xs

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile p (x : xs) = if p x then x : (takeWhile p xs) else []

drop :: Int -> [a] -> [a]
drop _ [] = []
drop n (x:xs)
  | n > 0 = drop (n - 1) xs
  | otherwise = x:xs

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ [] = []
dropWhile p (x:xs)
  | p x = dropWhile p xs
  | otherwise = x:xs

reverse :: [a] -> [a]
reverse [] = []
reverse [a] = [a]
reverse (x:xs) = reverse xs ++ [x]

rotate :: Int -> [a] -> [a]
rotate n xs
  | n <= 0    = xs
  | null xs   = []
  | otherwise = let len = length xs
                    rotationCount = n `mod` len
                in drop (len - rotationCount) xs ++ take (len - rotationCount) xs

lotate :: Int -> [a] -> [a]
lotate n xs
  | n <= 0    = xs
  | null xs   = []
  | otherwise = let rotationCount = n `mod` length xs
                in drop rotationCount xs ++ take rotationCount xs

type Generator a = (a -> a, a -> Bool, a)
fromGenerator :: Generator a -> [a]
fromGenerator (action, condition, start)
  | condition start = (action start) : fromGenerator (action, condition, (action start))
  | otherwise = []

replicate :: Int -> a -> [a]
replicate n x
  | n <= 0    = []
  | otherwise = x : replicate (n - 1) x

inits :: [a] -> [[a]]
inits [] = [[]]
inits xs = go 0
        where
          len = length xs
          go n
            | n > len = []
            | otherwise = take n xs : go (n + 1)

tails :: [a] -> [[a]]
tails [] = [[]]
tails xs = xs : tails (drop 1 xs)

-- Section 3: zips and products
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zip :: [a] -> [b] -> [(a, b)]
zipFill :: a -> b -> [a] -> [b] -> [(a, b)]
data ZipFail = ErrorFirst | ErrorSecond deriving (Eq, Show)
zipFail :: [a] -> [b] -> Either ZipFail [(a, b)]
unzip :: [(a, b)] -> ([a], [b])

-- Section 4: Knight travels
-- Position (0, 0) is the top-left corner.
data KnightPos = KnightPos {x :: Int, y :: Int} deriving (Show, Eq)
data KnightMove = TopLeft | TopRight | RightTop | RightBottom | BottomRight | BottomLeft | LeftBottom | LeftTop deriving (Enum, Bounded, Show, Eq)
-- Utility to get all knight moves. Don't worry about the implementation of this.
allKnightMoves :: [KnightMove]
allKnightMoves = [minBound .. maxBound]
data Board = Board {width :: Int, height :: Int} deriving (Show, Eq)
tour :: Board -> KnightPos -> Maybe [KnightMove]
newtype InvalidPosition = InvalidPosition KnightPos deriving (Show, Eq)
translate :: KnightPos -> [KnightMove] -> [KnightPos]
translate' :: [KnightPos] -> Either InvalidPosition [KnightMove]

-- Bonus (10 points)
mark :: Board -> [KnightPos] -> Either InvalidPosition [[Int]]
