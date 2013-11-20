import Data.Char
import Data.List

-- 1.1
concat3 s1 s2 s3
  | length s2 < 2 = s1 ++ s3
  | otherwise = s1 ++ s2 ++ s3

-- 1.2
showSalary salary bonus =
  if salary < 0
  then "This guy owes money to company"
  else
    if bonus /= 0
    then "Salary " ++ show salary ++ ", and bonus " ++ show bonus
    else "Salary " ++ show salary

-- 1.3
showDigit x
  | x == 0 = "zero"
  | x == 1 = "one"
  | x == 2 = "two"
  | x == 3 = "three"
  | x == 4 = "four"
  | x == 5 = "five"
  | x == 6 = "six"
  | x == 7 = "seven"
  | x == 8 = "eight"
  | x == 9 = "nine"
  | otherwise = "not valid"

showDecimalHelper1 x
  | x == 0 = ""
  | x == 1 = "ooookay"
  | x == 2 = "twenty "
  | x == 3 = "thirty "
  | x == 4 = "fourty "
  | x == 5 = "fifty "
  | x == 6 = "sixty "
  | x == 7 = "seventy "
  | x == 8 = "eighty "
  | x == 9 = "ninety "
  | otherwise = "not valid"

showDecimalHelper2 x
  | x == 0 = ""
  | x == 1 = "one"
  | x == 2 = "two"
  | x == 3 = "three"
  | x == 4 = "four"
  | x == 5 = "five"
  | x == 6 = "six"
  | x == 7 = "seven"
  | x == 8 = "eight"
  | x == 9 = "nine"
  | otherwise = "not valid"

showDecimalHelper3 x
  | x == 10 = "ten"
  | x == 11 = "eleven"
  | x == 12 = "twelve"
  | x == 13 = "thirteen"
  | x == 14 = "fourteen"
  | x == 15 = "fifteen"
  | x == 16 = "sixteen"
  | x == 17 = "seventeen"
  | x == 18 = "eighteen"
  | x == 19 = "nineteen"
  | otherwise = "not valid"

showDecimal x
  | x < 20 = showDecimalHelper3 x
  | otherwise = showDecimalHelper1 (div x 10) ++ showDecimalHelper2 (mod x 10)

-- 2.1
foobar s1
  | length s1 < 4 = error "not enough chars"
  | otherwise = tail $ take ((length s1) - 3) s1

-- 2.2
initials s1 s2 = [head s1] ++ ". " ++ [head s2] ++ "."

-- 2.3
concat4 s1 s2 =
  if length s1 > length s2
  then s1 ++ s2
  else s2 ++ s1

-- 2.4
safeHead l
  | null l = []
  | otherwise = [head l]

-- 2.5
hasDuplicates a = (length a) /= (length $ nub a)

-- 3.1
doublesFromTo a b = [2 * x | x <- [(minimum [a,b])..(maximum [a,b])]]

-- 3.2
succ' a
  | a == 'z' = 'a'
  | otherwise = succ a

convert n a
  | n == 0 = a
  | otherwise = convert (n-1) (succ' a)

ceasarCode n s = [(convert n (toLower c)) | c <- s, c /= ' ']


