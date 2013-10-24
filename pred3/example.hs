import Data.List
import Data.Char

-- third lecture class examples

concatThree :: String -> String -> String -> String
concatThree s1 s2 s3 = s1 ++ s2 ++ s3

number :: Int -> Int -> Int
number x y = x * 10 + y

trimBy :: Int -> String -> String
trimBy n xs = reverse $ drop n $ reverse $ drop n xs

