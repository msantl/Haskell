import Data.List
import Data.Char
import Prelude

-- 1

data Sex = Male | Female deriving (Show,Read,Eq,Ord)

data Person2 = Person2 {
  personId2 :: String,
  forename2 :: String,
  surname2  :: String,
  sex2      :: Sex,
  mother2   :: Maybe Person2,
  father2   :: Maybe Person2,
  partner2  :: Maybe Person2,
  children2 :: [Person2] } deriving (Show,Read,Eq,Ord)

sin2 = Person2 "125" "Aaaa" "Doe" Male Nothing Nothing Nothing []
sin1 = Person2 "124" "Bbbb" "Doe" Male Nothing Nothing Nothing []
john = Person2 "123" "John" "Doe" Male Nothing Nothing (Just jane) [sin1, sin2]
jane = Person2 "623" "Jane" "Fox" Female (Just ann) (Just ivan)  (Just john) [sin1, sin2]
lane = Person2 "624" "Lane" "Fox" Female (Just ann) (Just ivan)  Nothing []
ann  = Person2 "343" "Ann"  "Doe" Female Nothing Nothing Nothing [jane, lane]
ivan = Person2 "787" "Ivan" "Lol" Male   Nothing Nothing Nothing [jane, lane]

-- 1.1
partnersMother :: Person2 -> Maybe Person2
partnersMother p = case partner2 p of
    Nothing -> Nothing
    Just p -> mother2 p

-- 1.2
parentCheck :: Person2 -> Bool
parentCheck p = elem c cm && elem c cf
    where
        c = personId2 p
        cm = map personId2 $ maybe [] children2 $ mother2 p
        cf = map personId2 $ maybe [] children2 $ father2 p

-- 1.3
sister :: Person2 -> Maybe Person2
sister p
    | null sisters = Nothing
    | otherwise = Just (head sisters)
    where
        sisters = cm ++ cf
        cm = filter (\x -> sex2 x == Female && personId2 p /= personId2 x) $ maybe [] children2 $ mother2 p
        cf = filter (\x -> sex2 x == Female && personId2 p /= personId2 x) $ maybe [] children2 $ father2 p

-- 1.4
descendant :: Person2 -> [Person2]
descendant p = dfs (children2 p) (children2 p)
    where
        dfs []      ys   = ys
        dfs (x:xs)  ys  = (dfs xs zs) ++ (dfs (children2 x) [])
            where zs = ys ++ (children2 x)


-- 2
data MyList a = Empty | Cons a (MyList a) deriving (Show,Read,Ord)

infixr 5 -+-
(-+-) = Cons

lista = 1 -+- 2 -+- 3 -+- Empty
prazna_lista = Empty

-- 2.1
listHead :: MyList a -> Maybe a
listHead Empty = Nothing
listHead (Cons a _) = Just a

-- 2.2
listMap :: (a -> b) -> MyList a -> MyList b
listMap f Empty = Empty
listMap f (Cons a b) = Cons (f a) (listMap f b)

-- 3
data Tree a = Null | Node a (Tree a) (Tree a) deriving (Show)

intTree :: Tree Int
intTree = Node 1 (Node 2 Null Null) (Node 3 Null Null)

-- 3.1
treeMax :: Ord a => Tree a -> a
treeMax Null = error "Empty tree"
treeMax (Node a b c) = max (findTreeMax b a) (findTreeMax c a)
    where
        findTreeMax (Node a l r) curr = max (findTreeMax l (max a curr)) (findTreeMax r (max a curr))
        findTreeMax Null         curr = curr

-- 3.2
treeToList :: Ord a => Tree a -> [a]
treeToList Null = []
treeToList (Node a l r) = (treeToList l) ++ [a] ++ (treeToList r)

-- 3.3
levelCut :: Int -> Tree a -> Tree a
levelCut _ Null = Null
levelCut n (Node a l r)
    | n > 0 = Node a (levelCut (n-1) l) (levelCut (n-1) r)
    | otherwise = Null

-- 4

treeInsert :: Ord a => a -> Tree a -> Tree a
treeInsert x Null = Node x Null Null
treeInsert x t@(Node y l r)
  | x < y     = Node y (treeInsert x l) r
  | x > y     = Node y l (treeInsert x r)
  | otherwise = t

-- 4.1
listToTree :: Ord a => [a] -> Tree a
listToTree xs = foldl (\acc x -> treeInsert x acc) Null xs

-- 4.2
sortAndNub :: Ord a => [a] -> [a]
sortAndNub = treeToList . listToTree

-- 5
data Weekday =
  Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
  deriving (Show,Enum)

data Person = Person {
  idNumber :: String,
  forename :: String,
  surname  :: String,
  sex      :: Sex,
  age      :: Int,
  partner  :: Maybe Person,
  children :: [Person] } deriving (Read,Eq,Ord)

yesterday :: Weekday -> Weekday
yesterday = pred

dayAfterYesterday :: Weekday -> Weekday
dayAfterYesterday = succ . pred

workDays = [Monday .. Friday]

-- 5.1
instance Eq Weekday where
    Monday    == Monday    = True
    Tuesday   == Tuesday   = True
    Wednesday == Wednesday = True
    Thursday  == Thursday  = True
    Friday    == Friday    = False
    Saturday  == Saturday  = True
    Sunday    == Sunday    = True
    _         == _         = False

-- 5.2
instance Show Person where
    show (Person _ ime prezime _ _ _ _) = show (ime ++ " " ++ prezime)

-- 6

-- 6.1
instance Eq (MyList a) where
    _ == _ = False

-- 6.2
instance Eq (Tree a) where
    _ == _ = False

