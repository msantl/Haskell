import Data.List
import Data.Char
import Prelude

-- 1.1
sumEven :: [Integer] -> Integer
sumEven = sum . map snd . filter (\(x,y) -> odd x == False) . zip [0..]

-- 1.2
filterWords :: [String] -> String -> String
filterWords ws = unwords . filter (flip notElem ws) . words

-- 1.3
initials3 :: String -> (String -> Bool) -> String -> String
initials3 d p = concat . map (\x -> x : d) . map (toUpper . head) . filter p . words

initials :: String -> String
initials = initials3 "." (\x -> True)

-- 2.1
maxDiff :: [Int] -> Int
maxDiff xs = maximum $ map (uncurry (-)) $ zip xs (tail xs)

minDiff :: [Int] -> Int
minDiff xs = minimum $ map (uncurry (-)) $ zip xs (tail xs)

maxMinDiff :: [Int] -> (Int, Int)
maxMinDiff xs = (minDiff xs, maxDiff xs)

-- 2.2
studentPassed :: [(String, Double)] -> [String]
studentPassed xs = map (fst) $ filter (\x -> 2.0 * (snd x) >= maks) xs
    where maks = maximum $ map (snd) xs

-- 3.1
isTitleCased :: String -> Bool
isTitleCased xs = all (\x -> isUpper $ head x) (words xs)

-- 3.2
sortPairs :: Ord a => [(a, a)] -> [(a, a)]
sortPairs = sortBy drugi
    where
        drugi (_, x) (_, y)
            | x < y = LT
            | otherwise = GT

-- 3.3
filename :: String -> String
filename xs = reverse $ takeWhile ( /= '/') (reverse xs)

-- 3.4
maxElemIndices :: Ord a => [a] -> [Int]
maxElemIndices xs = findIndices (\x -> x == maks) xs
    where maks = maximum xs

-- 4.1
elem' :: Eq a => a -> [a] -> Bool
elem' x = foldr (\a b -> (a == x) || b) False

-- 4.2
reverse' :: [a] -> [a]
reverse' = foldr (\a b -> b ++ [a]) []

-- 4.3
nubRuns :: Eq a => [a] -> [a]
nubRuns = foldr (\x y -> if null y == False && head y == x then y else [x] ++ y) []

-- 5.1
reverse'' :: [a] -> [a]
reverse'' = foldl (\a b -> b : a) []

-- 5.2
sumEven' :: [Integer] -> Integer
sumEven' xs = foldl (\b (x, y) -> if odd y then x + b else b) 0 (zip xs [0..])

-- 5.3
maxUnzip :: [(Int, Int)] -> (Int, Int)
maxUnzip [] = error "empty list"
maxUnzip xs = foldl1 (mojMax) xs
    where
        mojMax (x, y) (a, b)
            | x > a && y > b = (x, y)
            | x > a && y <= b = (x, b)
            | x <= a && y > b = (a, y)
            | otherwise = (a, b)
