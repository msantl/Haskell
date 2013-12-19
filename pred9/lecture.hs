import Data.List
import Data.Char
import Prelude

import Data.Functor
import Data.Foldable

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Tree as T

-- 5
data Weekday =
    Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
    deriving (Show, Enum)

-- 5.1
instance Eq Weekday where
    Monday == Monday = True
    Tuesday == Tuesday = True
    Wednesday == Wednesday = True
    Thursday == Thursday = True
    Friday == Friday = False
    Saturday == Saturday = True
    Sunday == Sunday = True
    _  == _ = False

-- 5.2


-- 6
data MyList a = Empty | Cons a (MyList a) deriving (Show, Read, Ord)

list1 = (Cons 5 Empty)
list2 = (Cons 5 (Cons 4 Empty))

-- 6.1
instance Eq a => Eq (MyList a) where
    Empty == Empty = True
    (Cons a _) == (Cons b _) = a == b
    _ == _ = False

-- 6.2


-- 1
class Ageing a where
    currentAge  :: a -> Int
    maxAge      :: a -> Int
    makeOlder   :: a -> a

data Breed = Beagle | Husky | Pekingese deriving (Eq, Ord, Show, Read)
data Dog = Dog {
    dogName     :: String,
    dogBreed    :: Breed,
    dogAge      :: Int
} deriving (Eq, Ord, Show, Read)

instance Ageing Dog where
    currentAge = dogAge
    makeOlder d = d {dogAge = dogAge d + 1}
    maxAge d = case dogBreed d of
        Husky   -> 29
        _       -> 20

veryOld :: Ageing a => a -> Bool
veryOld x = 10 * currentAge x >= 8 * maxAge x

pas1 = Dog "fifi" Husky 10
pas2 = Dog "medo" Beagle 10

-- 1.1
compareRelativeAge :: (Ageing a, Ageing b) => a -> b -> Ordering
compareRelativeAge x y
    | relAgeX < relAgeY = LT
    | relAgeX > relAgeY = GT
    | otherwise = EQ
    where
        relAgeX = (fromIntegral $ currentAge x) / (fromIntegral $ maxAge x)
        relAgeY = (fromIntegral $ currentAge y) / (fromIntegral $ maxAge y)

-- 2
data Tree a = Null | Node a (Tree a) (Tree a) deriving (Show, Eq)

-- 2.1
class Takeable t where
    takeSome :: Int -> t a -> [a]

instance Takeable [] where
    takeSome 0 _        = []
    takeSome _ []       = []
    takeSome n (x:xs)   = x : (takeSome (n-1) xs)

instance Takeable Tree where
    takeSome _ Null         = []
    takeSome 0 _            = []
    takeSome n (Node a l _) = a : (takeSome (n-1) l)

-- 3

-- 3.1
-- mapOnTreeMaybe :: (a -> b) -> Tree (Maybe a) -> Tree ()

-- 4.1
-- sumPositive :: (Foldable t, Num a) => t a -> Int


