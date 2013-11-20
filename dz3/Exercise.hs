import Data.List
import Data.Char

median :: (Integral a, Fractional b) => [a] -> b
median [] = error "median: Empty List"
median xs 
	| odd l	= realToFrac $ ys !! h
	| otherwise = realToFrac (ys !! h + ys !! (h-1)) / 2
	where 	l = length xs
		ys = sort xs	
		h = l `div` 2

-- 1.1
headHunter :: [[a]] -> a
headHunter ((x:_):_) = x
headHunter (_:(x:_):_) = x
headHunter (_:_:(x:_):_) = x
headHunter _ = error "No head"

-- 1.2
firstColumn :: [[a]] -> [a]
firstColumn m = [ x | (x:_) <- m ]

-- 1.3
shoutOutLoud :: String -> String
shoutOutLoud xs = unwords [ f:f:f:s | (f:s) <- words xs ]

-- 2.1
pad :: String -> String -> (String, String)
pad (x:xs) (y:ys) = (toUpper x : adjust len xs, toUpper y : adjust len ys)
	where	len = max (length xs) (length ys)
		adjust n s = s ++ replicate (n - length s) ' '
-- 2.2
quartiles :: [Int] -> (Double, Double, Double)
quartiles xs = (first_q, second_q, third_q)
	where 	lista = sort xs
		first_q  = median $ fst $ splitAt (div (length lista) 2) lista
		second_q = median lista
		third_q  = median $ snd $ splitAt ((div (length lista) 2)+1) lista

-- 3.1
pad' :: String -> String -> (String, String)
pad' (x:xs) (y:ys) = 
	let 	len = max (length xs) (length ys)
		adjust n s = s ++ replicate (n - length s) ' '
	in 	(toUpper x : adjust len xs, toUpper y : adjust len ys)

-- 3.2
quartiles' :: [Int] -> (Double, Double, Double)
quartiles' xs = 
	let 	lista = sort xs
		first_q  = median $ fst $ splitAt (div (length lista) 2) lista
		second_q = median lista
		third_q  = median $ snd $ splitAt ((div (length lista) 2)+1) lista
	in 	(first_q, second_q, third_q)

-- 4
foo ::(Num a, Num b, Eq a, Eq b, Show c) => (a,b) -> [c] -> String
foo (x,y) (_:z:_) = 
	"The pair " ++ case ones of
		2 -> "contains two one's"
		1 -> "contains one one"
		0 -> "does not contain a single one"
	++ " and the second element of the list is " ++ show z

	where ones = 
		case (x,y) of
			(1,1) -> 2
			(_,1) -> 1
			(1,_) -> 1
			(_,_) -> 0



