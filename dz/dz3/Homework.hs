import Data.List
import Data.Char

-- 1
type SimilarityMetric a = a -> a -> Double

-- 1.a
absDiff a b = abs (a-b)

-- checkTriangleInequality - checks if the triangle inequality (AB + BC >= AC) holds for every triplet in given list
-- @param m - similarity metric
-- @param xs - list of elements of which we perform our check
-- @return Bool - True if holds, False otherwise
checkTriangleInequality :: SimilarityMetric a -> [a] -> Bool
checkTriangleInequality m xs = and [(m a b) + (m b c) >= (m a c) | a <- xs, b <- xs, c <- xs]

-- 1.b
-- zipN - zip function that operates on arbitrarily many arguments
-- @param xss - a list of lists containing elements on which we perform zip function
-- @return - a list of lists containing zipped elements
zipN :: [[a]] -> [[a]]
zipN xss
	| or [null x | x <- xss] = error "one of the list is empty"
	| and [not $ null $ tail x | x <- xss ] = [head x | x <- xss] : zipN [tail x | x <- xss]
	| otherwise = [[head x | x <- xss]]

-- 1.c
-- findSimilar - returns a list of n elements from given list l, that are most similar to a according to given metrices ms
-- @param l - fixed element
-- @param ls - list of elements
-- @param ms - list of metrices
-- @param n  - number of lists 
-- @return - list of n elements that are most similar to the fixed element l
findSimilar :: Ord a => a -> [a] -> [SimilarityMetric a] -> Int -> [[(Double, a)]]
findSimilar l ls ms n = 
	[ if i >= length ls then [] else [ (reverse $ sort [ (m l x, x) | x <- ls ]) !! i | m <- ms ] | i <- [0..(n-1)] ]

-- 2

-- 2.a
-- stringSim - computes the string similarity between A and B
-- @param xs - string A
-- @param ys - string B
-- @return Double - computed max similarity
stringSim :: String -> String -> Double
stringSim xs ys = 2 * (maxList [sim x y | x <- xss, y <- yss]) / n
	where 	yss = tail $ subsequences ys
		xss = tail $ subsequences xs
		n = realToFrac $ length xs + length ys
		sim as bs = sum [if fst x == snd x then 1 else 0 | x <- zip as bs] 
		maxList (f:fs) = if null fs then f else max f (maxList fs) 

-- 2.b
-- findRepresentative - finds the representative string among a list of strings
-- @param xss - list of strings 
-- @return String - a string from xss that has the max similarity to other strings from the list
findRepresentative :: [String] -> String
findRepresentative xss
	| null xss = error "empty list"
	| otherwise = snd $ last $ sort [ (sum[stringSim xs ys | ys <- xss, ys /= xs], xs)  | xs <- xss] 

-- 3

-- 3.a
-- rle - run length encoding
-- @param xs - string to be encoded
-- @return String - encoded string
rle :: String -> String
rle xs = unwords [ (show (length x)) ++ [head x] | x <- group xs]

-- 3.b
-- rleInverse - run length decoding
-- @param xs - string to be decoded
-- @return String - decoded string
rleInverse :: String -> String
rleInverse xs = concat [replicate (read $ getInt x) (last x) | x <- words xs]
	where 	getInt (x:xs)
			| isNumber x = [x] ++ getInt xs
			| otherwise = []

-- 3.c
-- bwt - Burrows-Wheeler transform
-- @param xs - string to be transformed
-- @return String - transformed string
bwt :: String -> String
bwt xs = [ last x | x <- sort $ rotateAll ys] 
	where 	ys = xs ++ "#"
		rotateAll s = [ let y = splitAt n s in (snd y) ++ (fst y) | n <- [1..(length s)]]

-- 4
-- easyEval - evaluates a given simple mathematical expression
-- @param xs - string that has to be evaluated
-- @return Int - result
easyEval :: String -> Int
easyEval xs = eval $ words xs

-- helper function that does the actual evaluation
eval :: [String] -> Int
eval (x:y:xs) = if y == "+" then (read x) + eval xs else (read x) - eval(xs)
eval [x] = read x

-- 5

-- 5.a
-- fromSeed - returns a pseudorandom number from seed
-- @param seed - seed
-- @return Int - pseudorandom number
fromSeed :: Int -> Int
fromSeed seed = mod (a * seed + b) m
	where 	a = 1664525
		b = 1013904223
		m = 2^32

-- 5.b
-- randoms - returns a list of pseudorandom numbers
-- @param seed - seed
-- @return - a list of pseudorandom integers
randoms :: Int -> [Int]
randoms seed = tail $ iterate fromSeed seed

-- 5.c
-- choose -- returns a random list element with a new seed
-- @param seed - seed
-- @param xs - list of elements
-- @return Tuple - a new seed and a random element from list
choose :: Int -> [a] -> (Int, a)
choose seed xs = (rand, xs !! i)
	where 	rand = fromSeed $ fromSeed seed
		i = mod rand (length xs)
	

-- 6
-- helper function that filters key/text inputs
filter' :: String -> String
filter' xs = [ toLower x | x <- xs, x /= ' ']

-- helper function that returns a char value from [0..26]
offset :: Char -> Int
offset c = ord c - ord 'a'

-- 6.a
-- encrypt - takes a key and text and returns the encrypted text
-- @param key1 - key
-- @param text1 - text
-- @return String - encrypted text
encrypt :: String -> String -> String 
encrypt key1 text1
	| null key1 = error "Key not valid"
	| filter' key1 /= key1 = error "Key not valid"
	| otherwise = [ chr ((ord 'a') + (mod ((offset pomak) + (offset znak)) 26)) | (znak, pomak) <- zip text key ]
	where 	key = take (length text) $ cycle key1
		text = filter' text1

-- 6.b
-- decrypt - takes a key and text and returns the decrypted text
-- @param key1 - key
-- @param text = text
-- @return String - decrypted text
decrypt :: String -> String -> String
decrypt key1 text
	| null key = error "Key not valid"
	| filter' key /= key = error "Key not valid"
	| otherwise = [ chr ((ord 'a') + (mod ((offset znak) - (offset pomak)) 26)) | (znak, pomak) <- zip text key ]
	where 	key = take (length text) $ cycle key1

-- 6.c
-- stripes - groups letters into n stripes
-- @param n - wanted number of stripes
-- @param xs - string we want to strip
-- @return - stripes of the given string
stripes :: Int -> String -> [String]
stripes n xs 
	| null xs = error "Empty string"
	| otherwise = [ [ znak | (indeks, znak) <- zip [1..] xs, mod indeks n == mod i n ] | i <- [1..n]]

-- 6.d
-- mostFrequent - returns the most frequent char from given string
-- @param xs - string
-- @return Char - most frequent char
mostFrequent :: String -> Char
mostFrequent xs 
	| null xs = error "Empty string"
	| otherwise = snd $ last $ sort [ (length g, head g) | g <- group $ sort xs]

-- 6.e
-- breakKey - tries to break the key with the assumption that the most frequent letter in each stripe is e
-- @param n - length of the key
-- @param xs - decrypted text
-- @return String - guessed key
breakKey :: Int -> String -> String
breakKey n xs 
	| null xs = error "Empty string"
	| otherwise = [ chr ((ord 'a') + (mod ((offset (mostFrequent x)) - (offset 'e')) 26)) | x <- stripes n xs]

-- 6.f
-- breakCipher - tries to break the cipher using the breakKey function
-- @param lo - starting length of the key
-- @param hi - ending length fof the key
-- @param text - decrypted text
-- @return - a list of tupels containing (key, encrypted text) pairs
breakCipher :: Int -> Int -> String -> [(String, String)]
breakCipher lo hi text  
	| null text = error "Empty string"
	| lo > hi = error "Bad range"
	| otherwise = [ let key = breakKey n text in (key, decrypt key text) | n <- [lo..hi]]


