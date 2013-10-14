import Data.Char
import Data.List

x = 2
inc x = x + 1
digits2Number x y = x * 10 + y

name = "a da nije kurcina"
letter = 'K'

condDec x = if x > 0 then x -1 else x

foo x = (if even x then x*2 else 2) + 1

foo' x = if even x then x*2 else 2 + 1

bigNumber x = if x >= 1000 then True else False

bigNumber' x = x >= 1000

merge s1 s2 = s1 ++ (if s1 < s2 then " is not " else " is ") ++ s2

merge2 s1 s2  | s1 < s2    = s1 ++ " is " ++ s2
              | otherwise  = s1 ++ " is not " ++ s2

grade score | score < 50 = 1
            | score < 63 = 2
            | score < 76 = 3
            | score < 89 = 4
            | otherwise  = 5

showSalary amount bonus
  | bonus /= 0 = "Salary is " ++ show amount ++ ", and a bonus " ++ show bonus
  | otherwise  = "Salary is " ++ show amount

l1 = [1, 2, 3]
l1' = (1:(2:(3:([]))))

l2 = [1,2,3] ++ [4,5,6]

listify x  = [x]
listify' x = x:[]

blanks = repeat ' '

padTo10 s= s ++ take (10 - length s) blanks

m1 = minimum [1, 2, 3, 4, 5]

intToChar x
  | x >= 65   = ['A'..] !! (x - 65)
  | otherwise = error "index should be at least 65"

r1 = and [True, True, False]

a = sort "zagreb"

doubles = [x*2 | x <- [1..100]]

doublesFromTo a b = [x * 2 | x <- [a..b]]

sums1 = [x + y | x <- [1..10], y <- [1..10]]

sums2 = [x + y | x <- [1..10], y <- [1..10], x < y]

sums3 = [x + y | x <- [1..10], y <- [1..10], x < y, odd x || even y]

lengths xss = [length xs | xs <- xss]

totalLength xss = sum $ lengths xss
