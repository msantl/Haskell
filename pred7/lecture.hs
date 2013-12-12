import Data.List
import Data.Char
import Prelude

-- 1.1
reverse' :: [a] -> [a]
reverse' = foldl (\x y -> y : x) []

-- 1.2
sumEven :: Num a => [a] -> a
sumEven xs = foldl (\x (i, y) -> if odd i then x + y else x) 0 (zip [0..] xs)

-- 1.3
-- try to finish these at home


data Tricolor = Red | Green | Blue deriving Show

warmColor :: Tricolor -> Bool
warnColor Red = True
warmColor _ = False

myColor = Red

data Shape =
      Circle Double Double Double
    | Rectangle Double Double Double Double
    deriving Show

isCircle :: Shape -> Bool
isCircle (Circle _ _ _ ) = True
isCircle _ = False

area :: Shape -> Double
area (Circle _ _ r) = r ^ 2 * pi
area (Rectangle x1 y1 x2 y2) = (abs $ x1 - x2) * (abs $ y1 - y2)

-- 2.1
data Date = Datum Integer Integer Integer deriving Show

showDate :: Date -> String
showDate (Datum day month year) = (show day) ++ "." ++ (show month) ++ "." ++ (show year)

-- 2.2
translate :: Point -> Shape2 -> Shape2
data Point = Point Double Double deriving Show
data Shape2 = Circle2 Point Double | Rectangle2 Point Point deriving Show

translate (Point x y) (Circle2 (Point x1 y1) r) = Circle2 (Point (x1 + x) (y1 + y)) r
translate (Point x y) (Rectangle2 (Point x1 y1) (Point x2 y2)) = Rectangle2 (Point (x1+x) (y1+y)) (Point (x2+x) (y2+y))

-- 2.3
inShape :: Shape2 -> Point -> Bool
inShape (Circle2 (Point x y) r) (Point x1 y1) = (x1 - x) ^ 2 + (y1 - y) ^ 2 < r ^ 2
inShape (Rectangle2 (Point x1 y1) (Point x2 y2)) (Point x y) = x1 < x && x < x2 && y1 < y && y < y2

-- 2.4
-- try finish these at home

data Level = Bachelor | Master | PhD deriving (Show, Eq)
data Student2 = Student2 String String String Level Double deriving Show

firstName2 :: Student2 -> String
firstName2 (Student2 f _ _ _ _) = f

lastName2 :: Student2 -> String
lastName2 (Student2 _ f _ _ _) = f

-- or

data Student = Student {
    firstName :: String,
    lastName :: String,
    studentId :: String,
    level :: Level,
    avgGrade :: Double
} deriving Show


bestStudent = Student {
    studentId = "0036458898",
    firstName = "John", lastName = "Doe",
    level = Master, avgGrade = 5.0
}

showStudent :: Student -> String
showStudent s = studentId s ++ " " ++ firstName s ++ " " ++ lastName s

showStudent2 :: Student -> String
showStudent2 s = intercalate " " [studentId s, firstName s, lastName s]

showStudent3 :: Student -> String
showStudent3 (Student {studentId=id, firstName=f, lastName=l}) = intercalate " " [id, f, l]

aboveStudents :: Double -> [Student] -> [Student]
aboveStudents x = filter ((>= x) . avgGrade)

bestStudent2 = bestStudent {avgGrade = 2.9}

-- 3.1
improveStudent :: Student -> Student
improveStudent s = if grade <= 4.0 then s {avgGrade = (grade + 1)} else s
    where grade = avgGrade s

-- 3.2
avgGradePerLevels :: [Student] -> (Double, Double, Double)
avgGradePerLevels ss = (bachelor, master, phd)
    where
        bachelor = avg $ filter (\s -> level s == Bachelor) ss
        master = avg $ filter (\s -> level s == Master) ss
        phd = avg $ filter (\s -> level s == PhD) ss
        avg xs = (foldl (\x (Student _ _ _ _ g) -> x + g) 0 xs) / (foldl (\x _ -> x + 1.0) 0.0 xs)

-- 3.3
-- 3.4

data OldLevels = Graduate | Doctorate deriving Show
data GeneralStudent a = Student3 String String String a Double deriving Show

type BolognaStudent = GeneralStudent Level
type FER1Student    = GeneralStudent OldLevels

data Employee = Employee {
    name :: String,
    salary :: Maybe Double
} deriving Show

showSalary :: Employee -> String
showSalary e = case salary e of
    Nothing -> "unknown"
    Just n -> show n ++ " kn"

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x


