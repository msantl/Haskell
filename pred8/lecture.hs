import Data.List
import Data.Char
import Prelude

-- 1.1
data MyTriplet a b c = MyTriplet {
    first :: a,
    second :: b,
    third :: c
} deriving Show

toTriplet :: MyTriplet a b c -> (a, b, c)
toTriplet (MyTriplet a b c) = (a, b, c)

-- 1.2
data Employee = Employee {
    name :: String,
    salary :: Maybe Double
} deriving Show

totalSalaries :: [Employee] -> Double
totalSalaries xs = sum [plata x | x <- xs]
    where
    plata x =
        case salary x of
            Just x -> x
            Nothing -> 0.0

-- 1.3


data Sex = Male | Female deriving (Show, Read, Eq, Ord)

data Person = Person {
    idNumber :: String,
    forename :: String,
    surname :: String,
    sex :: Sex,
    age :: Int,
    partner :: Maybe Person,
    children :: [Person]
} deriving (Show, Read, Eq, Ord)

pero = Person "2323" "Pero" "Peric" Male 45 (Just ana) [marko]
ana = Person "3244" "Ana" "Anic" Female 43 (Just pero) [marko]
marko = Person "4341" "Marko" "Peric" Male 22 Nothing []

-- partners forname

partnersForname :: Person -> Maybe String
partnersForname p = case partner p of
    Just k -> Just $ forename k
    Nothing -> Nothing

-- or

partnersForname' :: Person -> Maybe String
partnersForname' = fmap forename . partner

-- children of both partners
pairsChildren :: Person -> [Person]
pairsChildren p = nub $ children p ++ maybe [] children (partner p)

-- this wont work (unable to comapre recursive data types)

data Person2 = Person2 {
    personId2 :: String,
    forename2 :: String,
    surname2 :: String,
    sex2 :: Sex,
    mother2 :: Person2,
    father2 :: Person2,
    partner2 :: Maybe Person2,
    children2 :: [Person2]
} deriving (Show, Read, Eq, Ord)

-- 2.1
partnersMother :: Person2 -> Maybe Person2
partnersMother = fmap mother2 . partner2

-- 2.2
parentCheck :: Person2 -> Bool
parentCheck x = elem (personId2 x) (map personId2 $ children2 $ mother2 x) && elem (personId2 x) (map personId2 $ children2 $ father2 x)

-- 2.3
sister :: Person2 -> Maybe Person2
sister x
    | null sisters = Nothing
    | otherwise = Just $ head sisters
    where
        sisters = filter (\x -> sex2 x == Female) $ children2 $ mother2 x


data MyList a = Empty | Cons a (MyList a) deriving (Show, Read, Eq, Ord)

infixr 5 -+-
(-+-) = Cons

-- 3.1
listHead :: MyList a -> Maybe a
listHead Empty = Nothing
listHead (Cons a _) = Just a

-- 3.2
listMap :: (a -> b) -> MyList a -> MyList b
listMap _ Empty = Empty
listMap f (Cons x xs) = f x -+- listMap f xs

data Tree a = Null | Node a (Tree a) (Tree a) deriving (Show, Eq)

intTree :: Tree Int
intTree = Node 1 (Node 2 Null Null) (Node 3 Null Null)

sumTree :: Tree Int => Int
sumTree Null = 0
sumTree (Node x left right) = x + sumTree left + sumTree right


-- 4.1
treeMax :: Ord a => Tree a -> a
treeMax Null = error "Prazno stablo"
treeMax (Node x left right) = max (treeMax' left x) (treeMax' right x)
    where
        treeMax' (Node x left right) curr = max curr $ max (treeMax' right (max x curr)) (treeMax' left (max x curr))
        treeMax' Null curr = curr

-- 4.2
treeElems :: Ord a => Tree a -> [a]
treeElems Null = []
treeElems (Node x left right) = lijevo ++ [x] ++ desno
    where
        lijevo = treeElems left
        desno  = treeElems right

-- 4.3
levelCut :: Int -> Tree a -> Tree a
levelCut _ Null = Null
levelCut n (Node root left right)
    | n > 0 = (Node root (levelCut (n-1) left) (levelCut (n-1) right))
    | otherwise = Null


treeInsert :: Ord a => a -> Tree a -> Tree a
treeInsert x Null = Node x Null Null
treeInsert x t@(Node y l r)
    | x < y = Node y (treeInsert x l) r
    | x > y = Node y l (treeInsert x r)
    | otherwise = t


-- 5.1
listToTree :: Ord a => [a] -> Tree a
listToTree xs = foldl (\acc x -> treeInsert x acc) Null xs

-- 5.2
treeToList :: Tree a -> [a]
treeToList Null = []
treeToList (Node x left right) = (treeToList left) ++ [x] ++ (treeToList right)

-- 5.3
sortAndNub :: Ord a => [a] -> [a]
sortAndNub = nub . treeToList . listToTree

