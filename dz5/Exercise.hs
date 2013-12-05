import Data.Char
import Data.List
import Prelude

-- 1.1
takeThree :: [a] -> [a]
takeThree = (3 `take`)

dropThree :: [a] -> [a]
dropThree = (3 `drop`)

hundredTimes :: a -> [a]
hundredTimes = (100 `replicate`)

-- 1.2
index :: [a] -> [(Int, a)]
index = ([0..] `zip`)

index' :: [a] -> [(a, Int)]
index' = (`zip` [0..])

-- 1.3
divider :: Int -> [Char]
divider = (`replicate` '=')

-- 2.1
applyOnLast :: (a -> a -> a) -> [a] -> [a] -> a
applyOnLast f xs ys = f (last xs) (last ys)

addThree :: Num a => a -> a -> a -> a
addThree x y z = x + y + z

lastTwoPlus100 :: [Integer] -> [Integer] -> Integer
lastTwoPlus100 xs ys = addThree 100 (applyOnLast min xs ys) (applyOnLast max xs ys)

-- 2.2
applyManyTimes :: Int -> (a -> a) -> a -> a
applyManyTimes n f x
    | n <= 0 = x
    | otherwise = applyManyTimes (n-1) f (f x)

applyTwice :: (a -> a) -> a -> a
applyTwice = (2 `applyManyTimes`)

-- 3.1
listifylist :: [a] -> [[a]]
listifylist = map tolist
    where tolist x = [x]

-- 3.2
cutoff :: Int -> [Int] -> [Int]
cutoff n = map (`min` n)

-- 4.1
sumEvenSquares :: [Integer] -> Integer
sumEvenSquares xs = sum $ map (^2) $ filter even xs

-- 4.2
freq :: Eq a => a -> [a] -> Int
freq x xs = length $ filter (== x) xs

-- 4.3
freqFilter :: Eq a => Int -> [a] -> [a]
freqFilter n xs = filter (`freq'` xs) xs
    where freq' x xs = freq x xs >= n

-- 5.1
withinInterval :: Int -> Int -> [Int] -> [Int]
withinInterval n m = filter (\x -> not $ (x >= n && x <= m))

-- 5.2
sndColumn :: [[a]] -> [a]
sndColumn = map (\(f:s:rest) -> s)

-- 5.3
canonicalizePairs :: Ord a => [(a, a)] -> [(a, a)]
canonicalizePairs xs = map (\(a, b) -> ((min a b), (max a b))) $ filter (\(x, y) -> x /= y) xs

