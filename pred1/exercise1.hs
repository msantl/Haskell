import Data.Char
import Data.List

concat3 s1 s2 s3 = s1 ++ (if length s2 < 2 then "" else s2) ++ s3

showSalary3 salary bonus =
  if    salary < 0
  then  "this guy owes money to company"
  else
    if bonus /= 0
    then "Salary " ++ show salary ++ ", and bouns " ++ show bonus
    else "Salary " ++ show salary

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
  | otherwise = "wtf"

showDecimalHelper1 x
  | x == 0 = ""
  | x == 1 = "ooookay"
  | x == 2 = "twenty"
  | x == 3 = "thirty"
  | x == 4 = "fourty"
  | x == 5 = "fifty"
  | x == 6 = "sixty"
  | x == 7 = "seventy"
  | x == 8 = "eighty"
  | x == 9 = "ninety"
  | otherwise = "wtf"

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
  | otherwise = "wtf"

showDecimal x = showDecimalHelper1 (div x 10) ++ " " ++ showDecimalHelper2 (mod x 10)

