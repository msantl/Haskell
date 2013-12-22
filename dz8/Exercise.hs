import Prelude hiding (foldr,foldl,foldr1, all)
import Data.Functor
import Data.Foldable

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Tree as T

-- 1
data Sex = Male | Female deriving (Show,Read,Eq,Ord)
data Person = Person {
  idNumber :: String,
  forename :: String,
  surname  :: String,
  sex      :: Sex,
  age      :: Int,
  partner  :: Maybe Person,
  children :: [Person] } deriving (Show,Read,Eq,Ord)

data Breed = Beagle | Husky | Pekingese deriving (Eq,Ord,Show,Read)
data Dog = Dog {
  dogName  :: String,
  dogBreed :: Breed,
  dogAge   :: Int } deriving (Eq,Ord,Show,Read)

class Ageing a where
    currentAge  :: a -> Int
    maxAge      :: a -> Int
    makeOlder   :: a -> a

instance Ageing Dog where
    currentAge = dogAge
    makeOlder d = d {dogAge = dogAge d + 1}
    maxAge d = case dogBreed d of
        Husky   -> 29
        _       -> 20

ines = Person "2692" "Ines" "Seni" Female 16 Nothing []
dado = Person "2692" "Dado" "Dadić" Male 105 Nothing []

fifi = Dog "fifi" Husky 10
medo = Dog "medo" Beagle 10

-- 1.1
compareRelativeAge :: (Ageing a, Ageing b) => a -> b -> Ordering
compareRelativeAge x y
    | relAgeX < relAgeY = LT
    | relAgeX > relAgeY = GT
    | otherwise = EQ
    where
        relAgeX = (fromIntegral $ currentAge x) / (fromIntegral $ maxAge x)
        relAgeY = (fromIntegral $ currentAge y) / (fromIntegral $ maxAge y)

-- 1.2
class Nameable a where
    name :: a -> String

instance Nameable Person where
    name x = (forename x) ++ " " ++ (surname x)

instance Nameable Dog where
    name x = (dogName x) ++ " the Dog"

-- 2
data MyList a = Empty | Cons a (MyList a) deriving (Show,Read,Eq,Ord)
data Tree a = Null | Node a (Tree a) (Tree a) deriving (Show,Eq)

-- 2.1
class Takeable t where
    takeSome :: Int -> t a -> [a]

instance Takeable [] where
    takeSome 0  _       = []
    takeSome _  []      = []
    takeSome n  (x:xs)  = x : (takeSome (n-1) xs)

instance Takeable Tree where
    takeSome 0 _            = []
    takeSome _ Null         = []
    takeSome n (Node a l _) = a : (takeSome (n-1) l)

-- 2.2
class Headed t where
    headOf :: t a -> a
    headOff :: t a -> t a

instance Headed [] where
    headOf x
        | null x = error "Empty list"
        | otherwise = head x

    headOff x
        | null x = error "Empty list"
        | otherwise = tail x

instance Headed Tree where
    headOf Null = error "Empty Tree"
    headOf (Node a _ _) = a

    headOff Null = error "Empty Tree"
    headOff (Node a l _) = l

instance Headed Maybe where
    headOf Nothing = error "Nothing"
    headOf (Just x) = x

    headOff _ = Nothing

-- 3.1
tr = Node (Just 1) (Node (Just 2) Null Null) (Node Nothing Null Null)

instance Functor Tree where
    fmap _ Null = Null
    fmap f (Node x l r) = Node (f x) (fmap f l) (fmap f r)

mapOnTreeMaybe :: (a -> b) -> Tree (Maybe a) -> Tree (Maybe b)

mapOnTreeMaybe f Null = Null
mapOnTreeMaybe f (Node a l r) = Node (fmap f a) (mapOnTreeMaybe f l) (mapOnTreeMaybe f r)

-- 3.2
data RoseTree a = RoseEmpty | RoseTreeNode a [RoseTree a] deriving (Show, Eq, Ord)

rtr = RoseTreeNode 5 [RoseTreeNode 4 [RoseTreeNode 7 [RoseEmpty]], RoseTreeNode 1 [RoseEmpty], RoseTreeNode 6 [RoseEmpty]]

instance Functor RoseTree where
    fmap _ RoseEmpty = RoseEmpty
    fmap f (RoseTreeNode a xs) = RoseTreeNode (f a) [fmap f x | x <- xs]

-- 4.1
sumPositive :: (Foldable t, Num a, Ord a) => t a -> a
sumPositive = foldr1 (\x acc -> if x > 0 then acc + x else acc)

-- 4.2
size :: Foldable t => t a -> Int
size = foldr (\x acc -> acc + 1) 0

-- 4.3
eqElems :: (Foldable t, Eq a) => t a -> Bool
eqElems xs = fst $ foldr fun (True, ae) xs
    where
        fun x (b, acc) = if x == acc then (b, acc) else (False, acc)
        ae = foldr1 (\x acc -> acc) xs

-- 4.4
instance Foldable RoseTree where
    foldr f z (RoseEmpty) = z
    foldr f z (RoseTreeNode a []) = f a z
    foldr f z (RoseTreeNode a (x:xs)) = foldr f rest x
        where
            rest  = foldr f z (rtree)
            rtree = RoseTreeNode a xs

-- 5.1
toSet :: (Foldable t, Ord a) => t a -> S.Set a
toSet = foldr (\x acc -> S.insert x acc) S.empty

-- 5.2
indexWords :: String -> M.Map String [Int]
indexWords xs = foldr ubaci M.empty (zip [0..] (words xs))
    where
        ubaci (id, word) acc = M.insertWith (++) word [id] acc

