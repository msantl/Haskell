import Data.Char
import Data.List
import Data.Tuple

-- 1
-- Helper function that returns the first n elements from a given list
first :: Int -> [a] -> [a]
first n xs = reverse $ drop (x-n) $ reverse xs
	where x = length xs

-- crossOne: replaces the left part of the first list with the left part of the second list
--			 and the right part of the first list with the right part of the second list
crossOne :: Int -> [a] -> [a] -> ([a], [a])
crossOne n xs ys = (y1 ++ x2, x1 ++ y2)
	where
		x1 = first n xs
		x2 = drop n xs
		y1 = first n ys
		y2 = drop n ys

-- crossMany : swap elements at given indices
-- 			   all offsets are being ignored
--			   the longer list is trimmed down so it mathces the shorter one in length
crossMany :: [Int] -> [a] -> [a] -> ([a], [a])
crossMany ids xs ys = (x1, y1)
	where
		x1 = [if (elem id ids) then (ys !! id) else (xs !! id) | id <- [0..n]]
		y1 = [if (elem id ids) then (xs !! id) else (ys !! id) | id <- [0..n]]
		n = (min (length xs) (length ys)) - 1

-- 2
-- interlace :	returns a list that is filled alternately with elements from given lists
interlace :: [a] -> [a] -> [a]
interlace xs ys = concat[ [fst e] ++ [snd e] | e <- zip xs ys]

-- 3
-- indices : returns a list containing indices for a given list
indices :: [a] -> [Int]
indices xs = [snd e | e <- zip xs [0..]]

-- suffixes : returns a list containing suffixes of a given list
suffixes :: [a] -> [[a]]
suffixes xs = tails xs

-- prefix : checks if the first list forms a prefix for the second list
prefix :: Eq a => [a] -> [a] -> Bool
prefix xs ys 
	| null xs = True
	| head xs == head ys = prefix (tail xs) (tail ys)
	| otherwise = False

-- 4
type Dict k v = [(k, v)]

-- exists : checks if the given key exists in dictonary
exists :: Eq k => k -> Dict k v -> Bool
exists key dict = not $ null [x | x <- dict, fst(x) == key]

-- get : returns value for the given key, or shows an error message if the key is not present in dictionary
-- get :: (Show k, Eq k) => Dict k v -> k -> v
get dict key
	| exists key dict = head [snd x | x <- dict, fst(x) == key]
	| otherwise = error ("key " ++ (show key) ++ " not found")

-- insert : inserts the given (key, value) pair in dictionary, overwrite if the same key exists 
insert' :: Eq k => k -> v -> Dict k v -> Dict k v
insert' key value dict
	| exists key dict = [(key, value)] ++ [item | item <- dict, fst(item) /= key]
	| otherwise = [(key, value)] ++ dict

-- delete : remove item with given key from dictionary
delete' :: Eq k => k -> Dict k v -> Dict k v
delete' key dict
	| exists key dict = [item | item <- dict, fst(item) /= key]
	| otherwise = dict

-- 5
-- Helper function, isNumber
isNumber' :: String -> Bool
isNumber' xs = not $ or [not $ isDigit x | x <- xs]

-- sumNumbers : sums all numbers that appear in String
sumNumbers :: String -> Int
sumNumbers xs = sum [if isNumber' x then read x else 0 | x <- words xs]

-- 6
type Point = (Double, Double)
type Polygon = [Point]

-- dist : calculates the euclidian distance between two given points
dist :: Point -> Point -> Double
dist a b = sqrt (((fst a) - (fst b)) ^ 2 + ((snd a) - (snd b))^2)

-- onLineSegment : checks if the first point lies on the line segment defined with other two points
onLineSegment :: Point -> Point -> Point -> Bool
onLineSegment a b c = abs((dist b c) - (dist a b) - (dist a c)) < eps
	where eps = 0.00001

-- isValid : checks if the given polygon is valid
isValid :: Polygon -> Bool
isValid p = length p >= 3

-- perimeter : calculates the perimeter for a given polygon, if the polygon is not valid, it shows an error message
perimeter :: Polygon -> Double
perimeter p
	| not $ isValid p = error "Not a valid polygon"
	| otherwise = sum [dist (p !! (mod id n)) (p !! (mod (id+1) n)) | id <- [0..(n-1)]]
		where n = length p

-- onPolygonBorder : checks if a point is on given polygon border, if the polygon is not valid, it shows an error message
onPolygonBorder :: Point -> Polygon -> Bool
onPolygonBorder a p
	| not $ isValid	p = error "Not a valid polygon"
	| otherwise = or [onLineSegment a (p !! (mod id n)) (p !! (mod (id + 1) n)) | id <- [0..n]]
		where n = length p

-- areAdjecant : checks if two polygons are adjecant, if one of them is not valid, it shows an error message
areAdjecant :: Polygon -> Polygon -> Bool
areAdjecant p q 
	| not $ isValid p = error "First polygon is not a valid polygon"
	| not $ isValid q = error "Second polygon is not a valid polygon"
	| otherwise = or [dist a b < 1 | a <- p, b <- q]

-- getAdjecant : for a given polygon list, returns a list containing pairs of indices that tell us which polygons are adjecant
getAdjecant :: [Polygon] -> [(Int, Int)]
getAdjecant ps  
	| or [not $ isValid p | p <- ps] = error "Not a valid polygon"
	| otherwise = [(a, b) | a <- [0..n], b <- [0..n], (areAdjecant (ps !! a) (ps !! b)) && a < b]
		where n = (length ps) - 1

-- 7
-- partition : returns a list that contains elements that satisfy given predicate
partition' :: [a -> Bool] -> [a] -> [[a]]
partition' preds xs = [[x | x <- xs, pred x == True] | pred <- preds]

-- 8
-- swapAdjecant : swaps the order of adjecat words in string
swapAdjecant :: String -> String
swapAdjecant xs = unwords $ concat [[w !! (2*id + 1), w !! (2*id)] | id <- [0..n]]
	where 
		n = (div (length $ words xs) 2) - 1
		w = words xs

-- 9
type Alphabet = [Char]

-- alphabetSort : sorts the given string according the character order in alphabet
alphabetSort :: String -> Alphabet -> String
alphabetSort xs al 
	| (length $ nub al) /= length al = error "invalid alphabet"
	| or [not $ elem (toLower x) al | x <- xs] = error "incomplete alphabet"
	| otherwise = concat [[ch | x <- xs, x == ch] | ch <- alphabet]
		where alphabet = concat [[toUpper x, x] | x <- al]

-- 10
-- funEq : checks if two unary functions are equal on given domain
funEq :: Eq b => [a] -> (a -> b) -> (a -> b) -> Bool
funEq domena x y = not $ or [((x d) /= (y d)) | d <- domena]

-- funEq2 : checks if two binary functions are equal on given domain
funEq2 :: Eq c => [a] -> [b] -> (a -> b -> c) -> (a -> b -> c) -> Bool
funEq2 dom1 dom2 x y = not $ or [((y d1 d2) /= (x d1 d2)) | d1 <- dom1, d2 <- dom2]

-- tabulate : returns a list containing pairs with (x, f(x)) for given domain and function 
tabulate :: [a] -> (a -> b) -> [(a, b)]
tabulate domena x = [(d, x d) | d <- domena]

-- injective : checks if the given function has the injective propery on the given domain
injective :: Eq b => [a] -> (a -> b) -> Bool
injective domena f = (length $ nub [ f x | x <- domena ]) == length domena


