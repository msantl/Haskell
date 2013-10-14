import Data.Char
import Data.List

foobar s1 = tail $ take ((length s1) - 3) s1

initials s1 s2 = [head s1] ++ ". " ++ [head s2] ++ "."

concat4 s1 s2
  | length s1 < length s2 = s2 ++ s1
  | otherwise = s1 ++ s2
