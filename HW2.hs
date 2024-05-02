{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -Werror #-}
-- Refines the above, allowing for unused imports.
{-# OPTIONS_GHC -Wno-unused-imports #-}

module HW2 where

import Data.List (find, foldl')
import Prelude (Bool (..), Bounded (..), Char, Either (..), Enum (..), Eq (..), Int, Integer, Maybe (..), Num (..), Ord (..), Show (..), String, all, and, any, concat, concatMap, const, curry, div, elem, error, even, filter, flip, foldl, foldr, fst, id, length, lines, lookup, map, mod, not, notElem, null, odd, otherwise, product, snd, sum, uncurry, undefined, unlines, unwords, words, (!!), ($), (&&), (++), (.), (||))



-- Section 1.1: Basic Maybes
concatMaybeMap :: (a -> Maybe b) -> Maybe a -> Maybe b
concatMaybeMap = undefined
fromMaybe :: a -> Maybe a -> a
fromMaybe = undefined
maybe :: b -> (a -> b) -> Maybe a -> b
maybe = undefined
catMaybes :: [Maybe a] -> [a]
catMaybes = undefined
mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe = undefined
-- Section 1.2 Basic Eithers
concatEitherMap :: (a -> Either e b) -> Either e a -> Either e b
concatEitherMap = undefined
either :: (a -> c) -> (b -> c) -> Either a b -> c
either = undefined
mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft = undefined
catEithers :: [Either e a] -> Either e [a]
catEithers = undefined
mapEither :: (a -> Either e b) -> [a] -> Either e [b]
mapEither = undefined
partitionEithers :: [Either a b] -> ([a], [b])
partitionEithers = undefined
eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = undefined

-- Section 2: Lists
take :: Int -> [a] -> [a]
take n _
  | n <= 0    = []
take _ []     = []
take n (x:xs) = x : take (n - 1) xs


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
zipWith = undefined

zip :: [a] -> [b] -> [(a, b)]
zip = undefined

zipFill :: a -> b -> [a] -> [b] -> [(a, b)]
zipFill = undefined

data ZipFail = ErrorFirst | ErrorSecond deriving (Eq, Show)
zipFail :: [a] -> [b] -> Either ZipFail [(a, b)]
zipFail = undefined

unzip :: [(a, b)] -> ([a], [b])
unzip = undefined

-- Section 4: Knight travels
-- Position (0, 0) is the top-left corner.
data KnightPos = KnightPos {x :: Int, y :: Int} deriving (Show, Eq)
data KnightMove = TopLeft | TopRight | RightTop | RightBottom | BottomRight | BottomLeft | LeftBottom | LeftTop deriving (Enum, Bounded, Show, Eq)
-- Utility to get all knight moves. Don't worry about the implementation of this.
allKnightMoves :: [KnightMove]
allKnightMoves = [minBound .. maxBound]

moveOffset :: KnightMove -> (Int, Int)
moveOffset TopLeft     = (-1, -2)
moveOffset TopRight    = (1, -2)
moveOffset RightTop    = (2, -1)
moveOffset RightBottom = (2, 1)
moveOffset BottomRight = (1, 2)
moveOffset BottomLeft  = (-1, 2)
moveOffset LeftBottom  = (-2, 1)
moveOffset LeftTop     = (-2, -1)

data Board = Board {width :: Int, height :: Int} deriving (Show, Eq)

validPos :: Board -> KnightPos -> Bool
validPos (Board w h) (KnightPos x y) = x >= 0 && x < w && y >= 0 && y < h

applyMove :: KnightPos -> KnightMove -> KnightPos
applyMove (KnightPos x y) move = let (dx, dy) = moveOffset move in KnightPos (x + dx) (y + dy)

tour :: Board -> KnightPos -> Maybe [KnightMove]
tour = undefined

newtype InvalidPosition = InvalidPosition KnightPos deriving (Show, Eq)
translate :: KnightPos -> [KnightMove] -> [KnightPos]
translate start [] = [start]  -- Start position with no moves results in the start itself
translate start (m:ms) =
  let next = applyMove start m  -- Calculate next position based on the first move
  in start : translate next ms  -- Recursively build the list including the start and next positions

translate' :: [KnightPos] -> Either InvalidPosition [KnightMove]
translate' [] = Right []
translate' [_] = Right []
translate' (p1:p2:ps) = case findMove p1 p2 of
    Just move -> case translate' (p2 : ps) of
        Right moves -> Right (move : moves)
        Left err    -> Left err
    Nothing   -> Left (InvalidPosition p2)

findMove :: KnightPos -> KnightPos -> Maybe KnightMove
findMove (KnightPos x1 y1) (KnightPos x2 y2) =
  find (\move -> let (dx, dy) = moveOffset move
                     in (x1 + dx == x2) && (y1 + dy == y2))
       allKnightMoves

-- Bonus (10 points)
mark :: Board -> [KnightPos] -> Either InvalidPosition [[Int]]
mark = undefined
