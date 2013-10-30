import Data.Char
import Data.List
import Data.Tuple

-- 1
first :: Int -> [a] -> [a]
first n xs = reverse $ drop (x-n) $ reverse xs
	where x = length xs

creossOne :: Int -> [a] -> [a] -> ([a], [a])
crossOne n xs ys = (y1 ++ x2, x1 ++ y2)
	where
		x1 = first n xs
		x2 = drop n xs
		y1 = first n ys
		y2 = drop n ys

crossMany -> [Int] -> [a] -> [a] -> ([a], [a])
crossMany ids xs ys = (x1, y1)
	where
		x1 = [if (elem id ids) then (ys !! id) else (xs !! id) | id <- [0..n]]
		y1 = [if (elem id ids) then (xs !! id) else (ys !! id) | id <- [0..n]]
		n = (min (length xs) (length ys))

-- 2
interlace :: [a] -> [a] -> [a]
interlace xs ys = concat[ [fst e] ++ [snd e] | e <- zip xs ys]


-- 3
indices :: [a] -> [Int]
indices xs = [snd e | e <- zip xs [0..]]

suffixes :: [a] -> [[a]]
suffixes xs = [drop n xs | n <- [0..(length xs)]]

prefix :: Eq a => [a] -> [a] -> Bool
prefix xs ys = length xs == length pre
	where pre = [id | id <- [0..((length xs)-1)], (xs !! id) == (ys !! id)]

-- 4
type Dict k v = [(k, v)]

exists :: Eq k => k -> Dict k v -> Bool
exists key dict = not $ null [x | x <- dict, fst(x) == key]

get :: (Show k, Eq k) => Dict k v -> k -> v
get dict key
	| exists key dict = concat [x | x <- dict, fst(x) == key]
	| otherwise = error "key " ++ (show key) ++ " not found"

insert' :: Eq k => k -> v -> Dict k v -> Dict k v
insert' key value dict
	| exists key dict = (key, value) :: [item | item <- dict, fst(item) /= key]
	| otherwise = (key, value) :: dict

delete' :: Eq k => k -> Dict k v -> Dict k v
delete' key dict
	| exists key dict = [item | item <- dict, fst(item) /= key]
	| otherwise = dict

-- 5
sumNumbers :: String -> Int
sumNumbers xs = sum $ read $ words xs

-- 6
type Point = (Double, Double)
type Poligon = [Point]

dist :: Point -> Point -> Double
dist a b = (((fst a) - (fst b)) ^ 2 + ((snd a) - (snd b))^2) ^ 0.5

onLineSegment :: Point -> Point -> Point -> Bool
onLineSegment a b c = abs((dist b c) - (dist a b) - (dist a c)) < eps
	where eps = 0.00001

isValid :: Ploygon -> Bool
isValid p = length p >= 3

perimeter :: Polygon -> Double
perimeter p
	| not $ isValid p = error "Not a valid polygon"
	| otherwise = sum [dist (p !! (mod id n)) (p !! (mod (id+1) n)) | id <- [0..(n-1)]]
		where n = length p

onPolygonBorder :: Point -> Polygon -> Bool
onPolygonBorder a p
	| not $ isValid	p = error "Not a valid polygon"
	| otherwise = any [onLineSegment (p !! (mod id n)) (p !! (mod (id + 1) n)) (mod (id + 2) n) | id <- [0..n]]
		where n = lenght p

areAdjecant :: Polygon -> Polygon -> Bool
areAdjecant p q 
	| not $ isValid = error "First polygon is not a valid polygon"
	| not $ isValid = error "Second polygon is not a valid polygon"
	| otherwise = any [dist a b < 1 | a <- p, b <- q]

getAdjecant :: [Polygon] -> [(Int, Int)]
getAdjecant ps = 
	| any [not $ isValid p | p <- ps] = error "Not a valid polygon"
	| otherwise = [(a, b) | a <- [0..n], b <- [0..n], (areAdjecant (p !! a) (p !! b)) && a < b]
		where n = (length p) - 1

-- 7
partition' :: [a -> Bool] -> [a] -> [[a]]
partition' preds xs = [[x | x <- xs, pred x == True] | pred <- preds]

-- 8
swapAdjecant :: String -> String
swapAdjecant xs = unwords $ concat [[w !! (2*(id+1)), w !! (2*id)] | id <- [0..n]]
	where 
		n = div (length xs) 2
		w = words xs

-- 9
type Alhphabet = [Char]

alphabetSort :: String -> Alphabet -> String
alphabetSort xs al 
	| null al = error "invalid alphabet"
	| any [not $ elem (toLower x) al | x <- xs] = error "incomplete alphabet"
	| otherwise = error "ae"

