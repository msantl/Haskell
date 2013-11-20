import Data.List
import Data.Char

-- 1
-- commonly used functions

-- 1.a

map' :: (a -> b) -> [a] -> [b]
-- @param f		- function to be applied to elements of given list
-- @param xs	= list of elements
-- @return list - list of elements on which the function f was applied
map' _ [] = []
map' f (x:xs) = (f x) : map' f xs

-- 1.b
filter' :: (a -> Bool) -> [a] -> [a]
-- @param f		- function to be evaluated on elements from given list
-- @param xs	- list of elements
-- @return list - list of elements that satisfy given function
filter' _ [] = []
filter' f (x:xs) 
	| (f x) == True = x : filter' f xs
	| otherwise = filter' f xs

-- 1.c
iterate' :: (a -> a) -> a -> [a]
-- @param f		- function to be applied while iterating 
-- @param x		- starting element
-- @return list - list of elements which were created by applying function f to the previous one
iterate' f x = y : iterate' f y
	where y = f x

-- 1.d
intercalate' :: [a] -> [[a]] -> [a]
-- @param xs	- pattern
-- @param yss	- list of patterns
-- @return list - list containing patterns from yss that were concatenated together with pattern xs as delimiter
intercalate' xs [ys] = ys
intercalate' xs (ys:yss) = ys ++ xs ++ intercalate xs yss

-- 2
sumEvens :: Num a => [a] -> a
-- @param xs 	- list of elements
-- @return Num 	- sum of elements on even indices
sumEvens [] = 0
sumEvens [x] = x
sumEvens (x:y:xs) = x + sumEvens xs

-- 3
dup :: [a] -> [a]
-- @param xs	- list we want to expand
-- @return list - expanded list such that element on the index i in the original list now appears (i+1) times
dup xs = dup' 1 0 xs
	where
		dup' _ _ [] = []
		dup' n i zs
			| i < n = [y] ++ dup' n (i+1) zs
			| otherwise = [y] ++ dup' (n+1) 0 ys
				where (y:ys) = zs
		

-- 4
prime :: Integral a => a -> Bool
-- @param Integral 	- number we test for being prime
-- @return Bool 	- True if the number is prime, False otherwise
prime x = prime' 2 x
	where
		prime' n x
			| n >= x = True
			| mod x n == 0 = False
			| otherwise = prime' (n+1) x


-- 5
words' :: String -> [String]
-- @param xs	- string containing letters and whitespaces
-- @param list 	- list of strings from original string which were separated by whitespace
words' [] = []
words' xs = words'' [] xs
	where 
		words'' [] [] = []
		words'' ys [] = [ys]
		words'' [] (z:zs)
			| isSpace z = words'' [] zs
			| otherwise = words'' [z] zs
		words'' ys (z:zs)
			| isSpace z = [ys] ++ words'' [] zs
			| otherwise = words'' (ys ++ [z]) zs


-- 6
prefixCalculator :: String -> Double
-- @param xs 		- string to be evaluated
-- @result Double 	- evaluated result
prefixCalculator xs = prefixCalculator' [] [] (words xs) []
	where
		prefixCalculator' [] [] [] [] = 0.0
		prefixCalculator' [x] [] [] [1] = x

		prefixCalculator' (x:y:zs) (q:qs) yss (1:1:0:ds) 
			| isInfinite res = error "Division by zero"
			| otherwise = prefixCalculator' (res : zs) qs yss (1 : ds)
				where res = q y x

		prefixCalculator' num_stack op_stack (ys:yss) stack = case ys of
			"+" -> prefixCalculator' num_stack ((+) : op_stack) yss (0 : stack)
			"-" -> prefixCalculator' num_stack ((-) : op_stack) yss (0 : stack)
			"*" -> prefixCalculator' num_stack ((*) : op_stack) yss (0 : stack)
			"/" -> prefixCalculator' num_stack ((/) : op_stack) yss (0 : stack)
			_ 	-> prefixCalculator' ((read ys) : num_stack) op_stack yss (1 : stack)


-- 7
findSubsequence :: Eq a => [a] -> [a] -> [Int]
-- @param xs	- pattern
-- @param ys	- string in which we search for characters from pattern to appear
-- @return list	- list of indices where the characters appear
findSubsequence xs ys = findSubsequence' xs 0 ys
	where 
		findSubsequence' [] _ _ = []
		findSubsequence' xs _ [] = error "subsequence does not exist"

		findSubsequence' (x:xs) j (y:ys)
			| x == y = [j] ++ findSubsequence' xs (j+1) ys
			| otherwise = findSubsequence' (x:xs) (j+1) ys

-- 8
-- helper function which checks if we have a element in list
exist :: Eq a => a -> [a] -> Bool
exist x ys = or [x == y | y <- ys]

-- 8.a
nubRight :: Eq a => [a] -> [a]
-- @param xs 	- list of elements  
-- @return list - list of elements without duplicates
nubRight xs = nubRight' [] xs
	where
		nubRight' res [] = res
		nubRight' res (x:xs) 
			| exist x res = nubRight' res xs
			| otherwise = nubRight' (res ++ [x]) xs


-- 8.b
nubLeft :: Eq a => [a] -> [a]
-- @param xs 	- list of elements  
-- @return list - list of elements without duplicates
nubLeft xs = nubLeft' [] xs
	where
		nubLeft' res [] = res
		nubLeft' res (x:xs) 
			| exist x xs = nubLeft' res xs
			| otherwise = nubLeft' (res ++ [x]) xs


-- 9
median :: (Integral a, Fractional b) => [a] -> b
-- @param xs	- list of elements 
-- @return b	- median for the given list
median [] = 0.0
median [x] = realToFrac x
median [x, y] = (realToFrac $ x + y) / 2
median xs = median $ init $ tail xs

-- 10
type Vertex = Integer
type Graph = [(Vertex, Vertex)]

-- 10.a
isNbrWith :: Graph -> Vertex -> Vertex -> Bool
-- @param g		- list of edges which represent a graph
-- @param x		- edge x
-- @param y 	- edge y
-- @return Bool	- True if x and y are neighbours, False otherwise
isNbrWith g x y = or [(a, b) == v | v <- g]
	where 
		a = min x y
		b = max x y

-- 10.b
areConnected :: Graph -> Vertex -> Vertex -> Bool
-- @param g		- list of edges which represent a graph
-- @param x		- edge x
-- @param y 	- edge y
-- @return Bool	- True if x and y are connected, False otherwise
areConnected g x y = areConnected' g x y vs [x, y]
	where
		vs = nub $ concat [[x,y] | (x,y) <- g]
		areConnected' g x y vs flag
			| isNbrWith g x y = True
			| otherwise = or [(areConnected' g x v vs (v:flag)) && (areConnected' g v y vs (v:flag)) | v <- vs, not $ exist v flag]


