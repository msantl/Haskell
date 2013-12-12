import Data.List
import Data.Char
import Prelude

-- 1.1
sumEven :: [Integer] -> Integer
sumEven = sum . map snd . filter (\(x,y) -> odd x == False) . zip [0..]

-- 1.2
filterWords :: [String] -> String -> String
filterWords ws = unwords . filter (flip elem ws) . words

-- 1.3

