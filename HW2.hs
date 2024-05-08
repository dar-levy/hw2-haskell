{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -Werror #-}
-- Refines the above, allowing for unused imports.
{-# OPTIONS_GHC -Wno-unused-imports #-}

module HW2 where

import Data.List (find, foldl')
import Prelude (Bool (..), Bounded (..), Char, Either (..), Enum (..), Eq (..), Int, Integer, Maybe (..), Num (..), Ord (..), Show (..), String, all, and, any, concat, concatMap, const, curry, div, elem, error, even, filter, flip, foldl, foldr, fst, id, length, lines, lookup, map, mod, not, notElem, null, odd, otherwise, product, snd, sum, uncurry, undefined, unlines, unwords, words, (!!), ($), (&&), (++), (.), (||))


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
zipWith _ [] _ = []
zipWith _ _ [] = []
zipWith f (a : as) (b : bs) = (f a b) : zipWith f as bs

zip :: [a] -> [b] -> [(a, b)]
--zip _ [] = []
--zip [] _ = []
--zip (a : as) (b : bs) = (a, b) : zip as bs
zip = zipWith (\x y -> (x, y))

zipFill :: a -> b -> [a] -> [b] -> [(a, b)]
zipFill _ _ [] [] = []
zipFill def_a def_b (a : as) [] = (a, def_b) : zipFill def_a def_b as []
zipFill def_a def_b [] (b : bs) = (def_a, b) : zipFill def_a def_b [] bs
zipFill def_a def_b (a: as) (b : bs) = (a, b) : zipFill def_a def_b as bs

data ZipFail = ErrorFirst | ErrorSecond deriving (Eq, Show)
zipFail :: [a] -> [b] -> Either ZipFail [(a, b)]
zipFail [] [] = Right []
zipFail [] (_ : _) = Left ErrorFirst
zipFail (_ : _) [] = Left ErrorSecond
zipFail (a : as) (b : bs) = case zipFail as bs of
  Left err -> Left err
  Right seq -> Right ((a, b) : seq)

unzip :: [(a, b)] -> ([a], [b])
-- unzip xs = (leftList xs, rightList xs) where
--   leftList :: [(a, b)] -> [a]
--   leftList [] = []
--   leftList ((a, _) : as) = a : leftList as
--   rightList :: [(a, b)] -> [b]
--   rightList [] = []
--   rightList ((_, b) : bs) = b : rightList bs
unzip [] = ([], [])
unzip ((a, b) : seq) = (a : as, b : bs) where
  (as, bs) = unzip seq

-- Section 4: Knight travels
-- Position (0, 0) is the top-left corner.
data KnightPos = KnightPos {x :: Int, y :: Int} deriving (Show, Eq)
data KnightMove = TopLeft | TopRight | RightTop | RightBottom | BottomRight | BottomLeft | LeftBottom | LeftTop deriving (Enum, Bounded, Show, Eq)
-- Utility to get all knight moves. Don't worry about the implementation of this.
allKnightMoves :: [KnightMove]
allKnightMoves = [minBound .. maxBound]
data Board = Board {width :: Int, height :: Int} deriving (Show, Eq)
newtype InvalidPosition = InvalidPosition KnightPos deriving (Show, Eq)

head :: [a] -> a
head (x:_) = x
head _     = error "empty list"

tail :: [a] -> [a]
tail []     = error "tail: empty list"
tail (_:xs) = xs

translate :: KnightPos -> [KnightMove] -> [KnightPos]
translate (KnightPos x y) moves = tail $ reverse $ foldl' (\acc move -> translateMove move (head acc) : acc) [KnightPos x y] moves

translateMove :: KnightMove -> KnightPos -> KnightPos
translateMove TopLeft (KnightPos x y) = KnightPos (x - 2) (y - 1)
translateMove TopRight (KnightPos x y) = KnightPos (x + 2) (y - 1)
translateMove RightTop (KnightPos x y) = KnightPos (x + 1) (y - 2)
translateMove RightBottom (KnightPos x y) = KnightPos (x + 1) (y + 2)
translateMove BottomRight (KnightPos x y) = KnightPos (x + 2) (y + 1)
translateMove BottomLeft (KnightPos x y) = KnightPos (x - 2) (y + 1)
translateMove LeftBottom (KnightPos x y) = KnightPos (x - 1) (y + 2)
translateMove LeftTop (KnightPos x y) = KnightPos (x - 1) (y - 2)

translate' :: [KnightPos] -> Either InvalidPosition [KnightMove]
translate' [] = Right []
translate' [_] = Right []
translate' (pos1:pos2:rest) =
    case find (\move -> translateMove move pos1 == pos2) allKnightMoves of
        Just move -> case translate' (pos2:rest) of
                        Right moves -> Right (move:moves)
                        Left err -> Left err
        Nothing -> Left (InvalidPosition pos2)

-- Main function to find a Knight's tour
tour :: Board -> KnightPos -> Maybe [KnightMove]
tour (Board w h) start
    | w == 1 && h == 1 = Just []  -- Special case for 1x1 board
    | otherwise = go [start] []
  where
    totalPositions = w * h

    go :: [KnightPos] -> [KnightMove] -> Maybe [KnightMove]
    go visited moves
      | length visited == totalPositions = Just (reverse moves)
      | otherwise = foldl' (\acc move -> acc `mplus` tryMove move) Nothing allKnightMoves
      where
        currentPos = head visited

        tryMove :: KnightMove -> Maybe [KnightMove]
        tryMove move =
          let newPos = translateMove move currentPos
          in if isValidMove newPos then go (newPos : visited) (move : moves) else Nothing

        isValidMove :: KnightPos -> Bool
        isValidMove (KnightPos x y) = x >= 0 && x < w && y >= 0 && y < h && notElem (KnightPos x y) visited

mplus :: Maybe a -> Maybe a -> Maybe a
mplus Nothing y = y
mplus x _ = x


-- Bonus (10 points)

-- Returns a list of pairs of indexes from a list of KnightPos
-- Notice that the order og x and y is flipped because in KnightPos y is the rows and x is the columns
knightPosListToTuple :: [KnightPos] -> [(Int, Int)]
knightPosListToTuple [] = []
knightPosListToTuple ((KnightPos x y) : kps) = (y, x) : knightPosListToTuple kps

markAux :: Int -> Int -> [(Int, Int)] -> Either InvalidPosition [[Int]]
markAux height width indexes = if (x, y) /= (-1, -1) then Left (InvalidPosition (KnightPos x y))
  else Right (map (\i -> map (\j -> lookupValue (i, j)) [0..width-1]) [0..height-1])
  where
    getFirstInvalidPosition :: Int -> Int -> [(Int, Int)] -> (Int, Int)
    getFirstInvalidPosition _ _ [] = (-1, -1)
    getFirstInvalidPosition h w ((col, row) : kps) = if col >= h || row >= w then (col, row) else getFirstInvalidPosition h w kps
    (x, y) = getFirstInvalidPosition height width indexes
    lookupValue (i, j) = maybe (-1) fst (lookup (i, j) indexedList)
    indexedList :: [((Int, Int), (Int, Int))]
    indexedList = zip indexes (zip [0..] [0..])

mark :: Board -> [KnightPos] -> Either InvalidPosition [[Int]]
mark (Board h w) kps = if (i, j) /= (-1, -1) then Left (InvalidPosition (KnightPos i j))
  else markAux h w indexes
  where
    indexes = knightPosListToTuple kps
    findFirstDuplicate :: [(Int, Int)] -> Maybe (Int, Int)
    findFirstDuplicate [] = Nothing
    findFirstDuplicate (x : xs) = if any (\y-> x == y) xs then Just x else findFirstDuplicate xs
    (i, j) = fromMaybe (-1, -1) (findFirstDuplicate indexes)