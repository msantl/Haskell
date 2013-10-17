import Data.Char
import Data.Tuple
import Data.List

-- 1
splitAt' n xs = (take n xs, drop n xs)

-- 2
longerThan n xs
  | n >= 0 = not $ null $ drop n xs
  | otherwise = True

-- 3
palindrome xs =
  if length(xs) > 0
  then
    if head(xs) == last(xs)
    then True
    else False
  else
    True

-- 4
pangram xs
  | elem ' ' xs = (length $ tail $ sort $ nub xs) == 26
  | otherwise = (length $ sort $ nub xs) == 26

-- 5ab
rotl n xs
  | null xs = []
  | n > 0 = rotl (n-1) (tail xs ++ [head xs])
  | n < 0 = rotl (n+1) ([last xs] ++ init xs)
  | otherwise = xs

-- 6
put el n xs
  | n < 0 = put el (mod n $ length xs) xs
  | null xs = [el]
  | otherwise =
    if length(xs) <= n
    then put el (mod n $ length xs) xs
    else  (take n xs) ++ [el] ++ (drop (n+1) xs)

-- 7
median xs
  | null xs = 0.0
  | even $ length xs = ((sort xs !! ((div (length xs) 2) - 1)) + (sort xs !! (div (length xs) 2))) / 2
  | otherwise = sort xs !! (div (length xs) 2)

-- 8
intersect' xs ys
  | null ys = []
  | otherwise = [x | x <- xs, elem x ys]

difference xs ys = [x | x <- xs, not $ elem x ys]

-- 9
allTheSame xs = (length $ nub xs) == 1

malformedMatrix mat = not $ allTheSame [length xs | xs <- mat]

asymetricalMatrix mat = not $ allTheSame ([length xs | xs <- mat] ++ [length mat])

-- 9a
allGreaterThan n mat
  | malformedMatrix mat = error "malformed matrix"
  | otherwise = null [x | x <- concat mat, x <= n]

-- 9b
getElem pos mat
  | fst pos < 0 = error "indices out of bounds"
  | snd pos < 0 = error "indices out of bounds"
  | fst pos >= length mat = error "indices out of bounds"
  | snd pos >= length (mat !! 0) = error "indices out of bounds"
  | malformedMatrix mat = error "malformed matrix"
  | otherwise = mat !! (fst pos) !! (snd pos)

-- 9c
sumElems mat
  | malformedMatrix mat = error "malformed matrix"
  | otherwise = sum [x | x <- concat mat]

-- 9d
trace mat
  | malformedMatrix mat = error "malformed matrix"
  | asymetricalMatrix mat = error "asymetrical matrix"
  | otherwise = sum [getElem (x,x) mat | x <- [0..((length mat)-1)]]

-- 10
-- 10a
sumTree root = sum $ leaves root

-- 10b
outermost root = (head $ leaves root, last $ leaves root)

-- 10c
leaves (l, r) = [fst l, snd l, fst r, snd r]

-- 10d
maxLeaf root = maximum $ leaves root

-- 11
detectCycles graph = [(l, r) | (l,r) <- graph, l == r || (l < r &&  elem (r, l) graph)]

