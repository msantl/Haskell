import Data.List
import Data.Char

-- 1.1
product' :: Num a => [a] -> a

product' [] = 0
product' [x] = x
product' (x:xs) = x * product' xs

-- 1.2
headsOf :: [[a]] -> [a]

headsOf [] = []
headsOf [(y:ys)] = [y]
headsOf ((y:_):xs) = y : headsOf xs

-- 2.1
modMult :: Integral a => a -> a -> [a] -> [a]
modMult _ _ [] = []
modMult n m [x] = [mod (x * n) m]
modMult n m (x:xs) = (mod (x * n) m) : modMult n m xs

-- 2.2
addPredcessor :: Num a => [a] -> [a]

addPredcessor [] = []
addPredcessor [x] = [x]
addPredcessor (x:xs) = x : addPredcessor' x xs

addPredcessor' :: Num a => a -> [a] -> [a]
addPredcessor' x [y] = [x+y]
addPredcessor' x (y:ys) = (x+y) : addPredcessor' y ys

-- 3.1
equalTriplets :: Eq a => [(a,a,a)] -> [(a,a,a)]
equalTriplets [] = []
equalTriplets (x:xs) 
	| (a == b && b == c) = x : equalTriplets xs
	| otherwise = equalTriplets xs
		where (a, b, c) = x

-- 3.2
replicate' :: Int -> a -> [a]

replicate' 0 x = []
replicate' n x 
	| n < 0  = []
	| otherwise = x : replicate (n-1) x

-- 4.1
drop' :: Int -> [a] -> [a]

drop' _ [] = []
drop' 0 xs = xs
drop' n (x:xs) = drop' (n-1) xs
	
drop'' :: Int -> [a] -> [a]

drop'' _ [] = []
drop'' 0 xs = xs
drop'' n xs 
	| n < 0 = reverse $ drop' (-n) (reverse xs)
	| otherwise = drop' n xs

-- 4.2
takeFromTo :: Int -> Int -> [a] -> [a]
takeFromTo n1 n2 xs
	| n2 < n1 = error "Upper bound is smaller than lower bound"
	| otherwise = takeFromTo' 0 n1 n2 xs

takeFromTo' :: Int -> Int -> Int -> [a] -> [a]

takeFromTo' _ _ _ [] = []
takeFromTo' n n1 n2 (x:xs)
	| (n1 <= n && n <= n2) = x : takeFromTo' (n+1) n1 n2 xs
	| otherwise = takeFromTo' (n+1) n1 n2 xs

-- 5.1
eachThird :: [a] -> [a]
eachThird [] = []
eachThird [x] = [x]
eachThird [x,y] = [x]
eachThird (x:y:z:qs) = x : eachThird qs

-- 5.2
crossZip :: [a] -> [b] -> [(a, b)]

crossZip [] _ = []
crossZip _ [] = []
crossZip [x] _ = []
crossZip _ [y] = []
crossZip (x1:x2:xs) (y1:y2:ys) = [(x1, y2), (x2, y1)] ++ crossZip xs ys


