{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -Werror #-}
-- Refines the above, allowing for unused imports.
{-# OPTIONS_GHC -Wno-unused-imports #-}

module HW2 where

import Data.List (find, foldl')
import Prelude (Bool (..), Bounded (..), Char, Either (..), Enum (..), Eq (..), Int, Integer, Maybe (..), Num (..), Ord (..), Show (..), String, all, and, any, concat, concatMap, const, curry, div, elem, error, even, filter, flip, foldl, foldr, fst, id, length, lines, lookup, map, mod, not, notElem, null, odd, otherwise, product, snd, sum, uncurry, undefined, unlines, unwords, words, (!!), ($), (&&), (++), (.), (||))

----------------------------------------:--------
-- DO NOT MODIFY ANYTHING ABOVE THIS LINE !!! --
------------------------------------------------

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
data Board = Board {width :: Int, height :: Int} deriving (Show, Eq)
newtype InvalidPosition = InvalidPosition KnightPos deriving (Show, Eq)
-- Utility function to check if a move is valid from a given position
isValidMove :: Board -> KnightPos -> KnightMove -> Bool
isValidMove (Board w h) (KnightPos x y) move =
    let newX = x + dx
        newY = y + dy
        dx = case move of
            RightTop -> 1
            RightBottom -> 1
            BottomRight -> 2
            BottomLeft -> 2
            LeftBottom -> -1
            LeftTop -> -1
            TopLeft -> -2
            TopRight -> -2
        dy = case move of
            RightTop -> -2
            RightBottom -> 2
            BottomRight -> 1
            BottomLeft -> 1
            LeftBottom -> 2
            LeftTop -> -2
            TopLeft -> -1
            TopRight -> -1
    in newX >= 0 && newX < w && newY >= 0 && newY < h

-- Function to translate a list of moves into a list of positions
translate :: KnightPos -> [KnightMove] -> [KnightPos]
translate startPos moves = scanl (\(KnightPos x y) move ->
    case move of
        RightTop -> KnightPos (x + 1) (y - 2)
        RightBottom -> KnightPos (x + 1) (y + 2)
        BottomRight -> KnightPos (x + 2) (y + 1)
        BottomLeft -> KnightPos (x - 2) (y + 1)
        LeftBottom -> KnightPos (x - 1) (y + 2)
        LeftTop -> KnightPos (x - 1) (y - 2)
        TopLeft -> KnightPos (x - 2) (y - 1)
        TopRight -> KnightPos (x + 2) (y - 1)
    ) startPos moves

-- Function to translate a list of positions into a list of moves
translate' :: [KnightPos] -> Either InvalidPosition [KnightMove]
translate' [pos] = Right []
translate' (p1:p2:ps) =
    case find (\move -> isValidMove (Board 9999 9999) p1 move && p2 `elem` translate p1 [move]) allKnightMoves of
        Just move -> case translate' (p2:ps) of
            Right moves -> Right (move:moves)
            Left err -> Left err
        Nothing -> Left (InvalidPosition p2)

-- Function to check if a given tour is valid on the board
isValidTour :: Board -> [KnightMove] -> Bool
isValidTour board moves = all (uncurry (isValidMove board)) $ zip startPos (tail posList)
    where
        startPos = KnightPos 0 0 : map (\pos -> last $ translate pos moves) posList
        posList = scanl (\(KnightPos x y) move ->
            case move of
                RightTop -> KnightPos (x + 1) (y - 2)
                RightBottom -> KnightPos (x + 1) (y + 2)
                BottomRight -> KnightPos (x + 2) (y + 1)
                BottomLeft -> KnightPos (x - 2) (y + 1)
                LeftBottom -> KnightPos (x - 1) (y + 2)
                LeftTop -> KnightPos (x - 1) (y - 2)
                TopLeft -> KnightPos (x - 2) (y - 1)
                TopRight -> KnightPos (x + 2) (y - 1)
            ) (KnightPos 0 0) moves

-- Function to find a valid tour on the board
tour :: Board -> KnightPos -> Maybe [KnightMove]
tour board startPos = case find (isValidTour board) possibleTours of
    Just moves -> Just moves
    Nothing -> Nothing
    where
        possibleTours = filter (\moves -> head (translate startPos moves) == KnightPos 0 0) $ permutations allKnightMoves

-- Bonus (10 points)
mark :: Board -> [KnightPos] -> Either InvalidPosition [[Int]]
mark = undefined
