import Data.List hiding (insert, union, difference)
import Data.Char
import Prelude

-- 1

-- 1.a
-- fprime - checks whether a number is prime or not
-- @param a : Integral type check for being prime
-- @return  : True or False
fprime :: Integral a => a -> Bool
fprime x = foldr (\a b -> (mod x a /= 0) && b) True [2..(x-1)]

-- 1.b
-- lprime - checks whether a number is prime or not
-- @param a : Integral type check for being prime
-- @return  : True or Falselprime :: Integral a => a -> Bool
lprime x = all (\y -> (mod x y) /= 0) [2..(x-1)]

-- 1.c
-- rprime - checks whether a number is prime or not
-- @param a : Integral type check for being prime
-- @return  : True or Falserprime :: Integral a => a -> Bool
rprime x = rprime' x (x-1)
    where
        rprime' x y
            | y == 1        = True
            | mod x y == 0  = False
            | otherwise     = rprime' x (y-1)


-- 2
-- cycle' - Prelude.cycle wannabe
-- @param [a]   - list we want to cycle
-- @return      - infinite list having cycles of given one
cycle' :: [a] -> [a]
cycle' [] = error "cycle': empty list"
cycle' xs = foldr (\x y -> xs ++ y) [] [1..]

-- 3
-- mapMasked - maps different functions to different elements based on the
--             index returned by the indexing function
-- @param (a -> Int)    - indexing function
-- @param [a -> b]      - list of functions
-- @param [a]           - list of elements
-- @return [b]          - list of elements that were mapped with corresponding
--                        functions
mapMasked :: (a -> Int) -> [a -> b] -> [a] -> [b]
mapMasked f fs xs = [ (fs !! id) x | (x, id) <- ps]
    where
        ids = map f xs
        ps = zip xs ids

-- 4
-- mean - calculates the arithmetic mean of a list of elements
-- @param [a]   - list of elements
-- @return a    - mean of the given elements
mean :: Fractional a => [a] -> a
mean [] = 0 / 0
mean xs = (foldr (\x y -> x + y) 0 xs) / (genericLength xs)

-- 5
-- 5.a
-- ncomp - comoposes a list of functions into a single function
-- @param [a -> a]  - list of functions
-- @return (a -> a) - composition of functions
ncomp :: [a -> a] -> (a -> a)
ncomp [] = id
ncomp fs = foldr (\x y -> y . x) id fs

-- 5.b
-- fsort - sorts functions in ascending order by the average value for a given
--         domain
-- @param [a -> a]  - list of functions that need to sorted
-- @param [a]       - domain
-- @return [a -> a] - sorted list of functions
fsort :: (Num a, Ord a) => [a -> a] -> [a] -> [a -> a]
fsort fs domain = sortBy cmp fs
    where
        apply f d = sum [f b | b <- d]
        cmp f1 f2
            | apply f1 domain < apply f2 domain = LT
            | otherwise = GT

-- 5.c
-- compsort - sorts functions in ascending order by the average value they
--            return for a given domain, and returns their composition
-- @param [a -> a]  - list of functions that need to be sorted
-- @param [a]       - domain
-- @return (a -> a) - composition of sorted functions
compsort :: (Num a, Ord a) => [a -> a] -> [a] -> (a -> a)
compsort fs domain value = (ncomp $ fsort fs domain) value

-- 6
type Set a = a -> Bool

-- 6.a
-- empty - a function that creates an empty set
empty :: Set a
empty = (\x -> False)

-- 6.b
-- single - a function that creates a set containing a single element
single :: Eq a => a -> Set a
single x = (\y -> if x == y then True else False)

-- 6.c
-- union' - a function that returns the union of two sets
union' :: Set a -> Set a -> Set a
union' s1 s2 = (\y -> if s1 y || s2 y then True else False)

-- 6.d
-- difference - a function that returns the difference between two sets
difference :: Set a -> Set a -> Set a
difference s1 s2 = (\y -> if (s1 y) /= (s2 y) then True else False)


-- 6.e
-- insert' - a function that inserts an element into the set
insert' :: Eq a => a -> Set a -> Set a
insert' x s1 = (\y -> if s1 y || x == y then True else False)


-- 6.f
-- a function that removes an element from the set
remove :: Eq a => a -> Set a -> Set a
remove x s1 = (\y -> if s1 y && x /= y then True else False)

-- 7
-- reduce - reduces the given list using f1 and f2
-- @param [[a]]         - non-empty two-level nested list
-- @param (a -> a -> a) - function f1
-- @param (a -> a -> a) - function f2
-- @return a            - reduced list
reduce :: [[a]] -> (a -> a -> a) -> (a -> a -> a) -> a
reduce xss f g
    | or [null xs | xs <- xss] = error "List must not be empty - dont know what to return"
    | length ys <= 1 = error "List must contain at least two lists - dont know what to return!"
    | otherwise = foldr1 f ys
    where
        ys = [foldr1 g y | y <- xss]

-- 8
-- 8.a
-- insert'' - the function takes an element and a list and inserts the element
--           into the list at the first position where the newly inserted
--           element will be less than or equal to the next element
-- @param a     - element that needs to be inserted
-- @param [a]   - list where we need to insert that element
-- @return [a]  - list containing the new element
insert'' :: Ord a => a -> [a] -> [a]
insert'' x xs
    | null xs           = [x]
    | (maximum xs) <= x = xs ++ [x]
    | otherwise         = foldl ubaci [] xs
    where
        ubaci acc y
            | elem x acc    = acc ++ [y]
            | x <= y        = acc ++ [x] ++ [y]
            | otherwise     = acc ++ [y]

-- 8.b
-- insertionSort - sorts a list of elements
-- @param [a]   - list that needs to be sorted
-- @return [a]  - sorted list
insertionSort :: Ord a => [a] -> [a]
insertionSort = foldl (\acc x -> insert' x acc) []

