import Data.List
import Data.Char
import Prelude

succOfFst :: (Int, Int) -> Int
-- succOfFst p = succ $ fst p
-- or
succOfFst = succ . fst

applyTwice :: (a -> a) -> a -> a
applyTwice f = f . f

cesarCode1 :: String -> String
-- the ole one
-- cesarCode1 s = [succ c | c <- s, c /= ' ']
-- filer and map
-- cesarCode1 s = map succ $ filter (/= ' ') s
-- with eta reduction and composition
cesarCode1 = map succ . filter ( /= ' ')

wordsSort :: String -> String
wordsSort = unwords . sort . words

initials :: String -> String
initials = map toUpper . map head . words

increasePositives :: [Integer] -> [Integer]
increasePositives = map (+1) . filter (>0)

tokenize :: String -> [String]
tokenize = filter (\w -> length w >= 1) . words . map toUpper

counts :: Ord a => [a] -> [(a, Int)]
counts = map (\xs@(x:_) -> (x, length xs)) . group . sort

-- exercise 1
sumEven :: [Integer] -> Integer
sumEven = sum . map fst . filter (\(x,y) -> odd x) . zip [0..]

filterWords :: [String] -> String -> String
filterWords ws = unwords . filter (not . flip elem ws) . words

initials3 :: String -> (String -> Bool) -> String -> String
initials3 d p = concat . map (:d) . map (toUpper . head) . filter p . words

-- exercise 2
maxDiff :: [Int] -> Int
maxDiff xs =  maximum $ map (uncurry (-)) $ zip xs (tail xs)

minDiff :: [Int] -> Int
minDiff xs =  minimum $ map (\(x,y) -> x - y) $ zip xs (tail xs)

maxMinDiff xs = (minDiff xs, maxDiff xs)

-- studentPassed :: [(String, Float)] -> [String]
-- vrati listu studenta koji imaju barem 50% od najboljeg

-- exercise 3
isTitleCased :: String -> Bool
isTitleCased xs = and [isUpper $ head s | s <- words xs]
-- all isUpper . map head . words

sortPairs :: Ord a => [(a,a)] -> [(a,a)]
sortPairs = sortBy drugi
    where drugi (_, x) (_, y)
            | x < y = LT
            | otherwise = GT



