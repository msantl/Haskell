import Data.List
import Data.Tuple
import Data.Char

ws = words "I think we agree, the past is over"
ls = lines "First line\nsecond line"
stream = unlines ls

capitalized s = [w | w <- words s, isUpper $ head w]

camelCase s = concat [toUpper (head w) : tail w | w <- words s]

camelCase' s = concat [toUpper g : r | (g:r) <- words s]

-- 4.1
totalLength s = sum [length xs | xs <- s]

letterCount s = totalLength $ [a | a <- words s, (length a) > 2]

-- 4.2
isPalindrome s = reverse nasa == nasa
  where nasa = [toLower a | a <- s, a /= ' ']

-- 5.1
inCircle r p s = [(x,y) | x <- [-10..10], y <- [-10..10], (x-p)^2 + (y-s)^2 < r^2]

-- 5.2
steps xs = zip xs (tail xs)

--

index xs = zip [1..] xs

evenElems xs = [snd x | x <- index xs, even (fst x)]

evenElems' xs = [x | (i, x) <- index xs, even i]

lista = zip3 [1..2] [2..3] [3..4]

-- 6.1
indices x xs = [fst y | y <- index xs, snd y == x ]

-- 6.2
showLineNumbers s = unlines $ [(show $ fst x) ++ snd x | x <- parovi ]
  where parovi = zip [1..] $ lines s

-- 6.3

common xs ys = [x | x <- index xs, y <- index ys, (fst x) == (fst y) && (snd x) == (snd y)]

haveAlignment xs ys = not $ null (common xs ys)
