{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

import Prelude hiding (foldr,foldl,foldr1, all, elem, zipWith, sum, maximum)
import qualified Data.List as L
import qualified Data.Char as C
import Data.Functor
import Data.Foldable hiding (toList, fromList)
import Data.Maybe

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Tree as T

-- 1
json :: JSON a => a -> String

class JSON a where
    showJSON :: a -> String

instance JSON Bool where
    showJSON x
        | x == True = "true"
        | otherwise = "false"

instance JSON Int where
    showJSON x = show x

instance JSON Double where
    showJSON x = show x

instance JSON String where
    showJSON x = show x

instance JSON [String] where
    showJSON xs = "[" ++ init (foldr (\x acc -> ((process x) ++ acc)) [] xs) ++ "]"
        where
            process x = (showJSON x) ++ ","

instance JSON (M.Map String Int) where
    showJSON xs = "{" ++ (init $ process xs) ++ "}"
        where
            process mapa = foldr (\x acc -> (show x) ++ ":" ++ val (M.lookup x mapa) ++ "," ++ acc) [] (M.keys mapa)
            val x = case x of
                Just s  -> show s
                Nothing -> error "Nema nista!"

json x = showJSON x

-- 2
class BinaryTree t where
    leftTree    :: t a -> Maybe (t a)
    rightTree   :: t a -> Maybe (t a)
    rootValue   :: t a -> Maybe a
    leaf        :: a -> t a
    node        :: a -> (t a) -> (t a) -> t a

-- 2.a
data Tree1 a = Leaf1 a | Node1 a (Tree1 a) (Tree1 a) deriving (Show)
data Tree2 a = Leaf2 a | Node2 (Tree2 a) (Tree2 a) deriving (Show)

t1 :: Tree1 Int
t1 = Node1 1 (Node1 2 (Leaf1 3) (Leaf1 4)) (Node1 5 (Leaf1 6) (Leaf1 7))
t2 :: Tree2 Int
t2 = Node2 (Leaf2 1) (Node2 (Leaf2 2) (Leaf2 3))

instance BinaryTree Tree1 where
    leftTree (Leaf1 a)              = Nothing
    leftTree (Node1 _ x _)          = Just x

    rightTree (Leaf1 a)              = Nothing
    rightTree (Node1 _ _ x)          = Just x

    rootValue   (Leaf1 x)       = Just x
    rootValue   (Node1 x _ _)   = Just x

    leaf        = Leaf1

    node x l r  = Node1 x l r

instance BinaryTree Tree2 where
    leftTree (Leaf2 a)              = Nothing
    leftTree (Node2 x _)            = Just x

    rightTree (Leaf2 a)             = Nothing
    rightTree (Node2 _ x)           = Just x

    rootValue   (Node2 _ _)     = Nothing
    rootValue   (Leaf2 x)       = Just x

    leaf        = Leaf2

    node _ l r  = Node2 l r

-- 2.b
isLeaf :: BinaryTree t => t a -> Bool
isLeaf bt
    | (isNothing $ leftTree bt) && (isNothing $ rightTree bt)   = True
    | otherwise                                                 = False

-- 2.c
isBranching :: BinaryTree t => t a -> Bool
isBranching bt
    | isLeaf bt = False
    | otherwise = isBranching_ bt
    where
        isBranching_ bt
            | isLeaf bt                                             = True
            | (isJust $ leftTree bt) && (isJust $ rightTree bt)     = (isBranching_ $ fromJust $ leftTree bt) && (isBranching_ $ fromJust $ rightTree bt)
            | otherwise                                             = False

-- 2.d
preorder :: BinaryTree t => t a -> [a]
preorder bt
    | (isJust $ leftTree bt)    && (isJust $ rightTree bt)      = val ++ (preorder left) ++ (preorder rajt)
    | (isJust $ leftTree bt)    && (isNothing $ rightTree bt)   = val ++ (preorder left)
    | (isNothing $ leftTree bt) && (isJust $ rightTree bt)      = val ++ (preorder rajt)
    | otherwise                                                 = val
    where
        left    = fromJust $ leftTree bt
        rajt    = fromJust $ rightTree bt
        val     = case (rootValue bt) of
            Just x  -> [x]
            Nothing -> []

-- 2.e
inorder :: BinaryTree t => t a -> [a]
inorder bt
    | (isJust $ leftTree bt)    && (isJust $ rightTree bt)      = (inorder left) ++ val ++ (inorder rajt)
    | (isJust $ leftTree bt)    && (isNothing $ rightTree bt)   = (inorder left) ++ val
    | (isNothing $ leftTree bt) && (isJust $ rightTree bt)      = val ++ (inorder rajt)
    | otherwise                                                 = val
    where
        left    = fromJust $ leftTree bt
        rajt    = fromJust $ rightTree bt
        val     = case (rootValue bt) of
            Just x  -> [x]
            Nothing -> []

-- 2.f
postorder :: BinaryTree t => t a -> [a]
postorder bt
    | (isJust $ leftTree bt)    && (isJust $ rightTree bt)      = (postorder left) ++ (postorder rajt) ++ val
    | (isJust $ leftTree bt)    && (isNothing $ rightTree bt)   = (postorder left) ++ val
    | (isNothing $ leftTree bt) && (isJust $ rightTree bt)      = (postorder rajt) ++ val
    | otherwise                                                 = val
    where
        left    = fromJust $ leftTree bt
        rajt    = fromJust $ rightTree bt
        val     = case (rootValue bt) of
            Just x  -> [x]
            Nothing -> []

-- 3
class Vector v where
    empty           :: v
    zipWith         :: (Double -> Double -> Double) -> v -> v -> v
    add             :: v -> v -> v
    dot             :: v -> v -> Double
    fromList        :: [Double] -> v
    toList          :: v -> [Double]
    fromAssocList   :: [(Int, Double)] -> v
    toAssocList     :: v -> [(Int, Double)]
    nonzeros        :: v -> [Double]

-- 3.a
    add a b     = zipWith (+) a b
    dot a b     = foldr (+) 0.0 (toList $ zipWith (*) a b)
    nonzeros x  = filter (/=0) (toList x)

-- 3.b
data ListVector = LV [Double] deriving Show

instance Vector ListVector where
    empty = LV []

    zipWith f (LV [])     (LV [])       = empty
    zipWith f (LV [])     (LV (y:ys))   = LV ((f 0 y) : (toList $ zipWith f empty (LV ys)))
    zipWith f (LV (x:xs)) (LV [])       = LV ((f x 0) : (toList $ zipWith f (LV xs) empty))
    zipWith f (LV (x:xs)) (LV (y:ys))   = LV ((f x y) : (toList $ zipWith f (LV xs) (LV ys)))

    fromList = LV

    toList (LV xs) = foldr (:) xs []

    fromAssocList xs = fromAssocList_ xs 0
        where
            fromAssocList_ []     _ = empty
            fromAssocList_ l@(x:xs) n
                | fst x == n = LV ((snd x) : (toList $ fromAssocList_ xs (n+1)))
                | otherwise  = LV (0.0 : (toList $ fromAssocList_ l (n+1)))

    toAssocList v = filter (\x -> snd x /= 0) $ zip [0..] (toList v)

-- 3.c
data SparseVector = SV (M.Map Int Double) deriving Show

instance Vector SparseVector where
    empty = SV (M.empty)

    zipWith f (SV x) (SV y) = fromAssocList lista
        where
            kljucevi = (M.keys x) ++ (M.keys y)
            lista = [ (i, f (M.findWithDefault 0.0 i x) (M.findWithDefault 0.0 i y) ) | i <- kljucevi ]

    fromList xs = SV (M.fromList [ (i, x) | (i, x) <- zip [0..] xs, x /= 0 ])

    toList (SV xs) = [M.findWithDefault 0 i xs | i <- [1..x] ]
        where x = if null $ M.keys xs then 0 else maximum $ M.keys xs

    fromAssocList xs = SV (M.fromList xs)

    toAssocList (SV xs) = M.toList xs

-- 3.d
norm :: Vector v => v -> Double
norm v = sqrt $ dot v v

-- 3.e
cosine :: Vector v => v -> v -> Double
cosine v1 v2
    | n1 == 0 || n2 == 0    = 0
    | otherwise             = (dot v1 v2) / (n1 * n2)
    where
        n1 = norm v1
        n2 = norm v2

-- 4
class Vectorizable a where
    vectorize :: a -> SparseVector

-- 4.a
counts :: Ord a => [a] -> [(a, Int)]
counts xs = [(head x, length x) | x <- (L.group $ L.sort xs)]

-- 4.b
instance Vectorizable [Char] where
    vectorize xs = fromAssocList $ translate $ counts xs
        where
            translate []        = []
            translate (y:ys)    = (C.ord $ fst y, fromIntegral $ snd y) : (translate ys)

-- 4.c
instance Vectorizable (Tree1 Int) where
    vectorize bt = fromAssocList [ (x, fromIntegral y) | (x, y) <- zip [0..] (preorder bt)]

-- 4.d
t3 = Node1 5 (Leaf1 1) (Leaf1 5) :: Tree1 Int
t4 = Leaf1 5 :: Tree1 Int

search :: Vectorizable a => a -> [a] -> [a]
search x xs = map snd $ L.sortBy cmp [(cosine (vectorize x) (vectorize y), y) | y <- xs]
    where
        cmp a b
            | fst a > fst b = LT
            | otherwise     = GT


-- 5
type Line = [Token]
data Token = Word String | Blank | HypWord String deriving (Eq, Show)

text = "He who controls the past controls the future. He who controls the present controls the past."

-- 5.a
string2line :: String -> Line
string2line xs = [Word x | x <- words xs]

-- 5.b
line2string :: Line -> String
line2string xs = L.unwords [x | Word x <- xs]

-- 5.c
tokenLength :: Token -> Int
tokenLength (Word xs)       = length xs
tokenLength (Blank)         = 1
tokenLength (HypWord xs)    = 1 + length xs

-- 5.d
lineLength :: Line -> Int
lineLength xs = sum [tokenLength x | x <- xs]

-- 5.e
breakLine :: Int -> Line -> (Line, Line)

breakLine n xs = breakLine_ n xs []
    where
        breakLine_ n l@(x:xs) ys
            | tokenLength x < n = breakLine_ (n - (tokenLength x)) xs (ys ++ [x])
            | otherwise         = (ys, l)

        breakLine_ _ [] ys = (ys, [])

-- 5.f
mergers :: [String] -> [(String, String)]
mergers xs = [ concat' $ splitAt i xs | i <- [1..((length xs) - 1)] ]
    where
        concat' x = (L.concat $ fst x, L.concat $ snd x)

-- 5.g
type HypMap = M.Map String [String]

enHyp :: HypMap
enHyp = M.fromList [
    ("controls", ["co", "nt", "ro" ,"ls"]),
    ("future", ["fu", "tu", "re"]),
    ("present", ["pre", "se", "nt"]) ]

hyphenate :: HypMap -> Token -> [(Token, Token)]
hyphenate mapa (Word xs) = map (\(x, y) -> (HypWord x, Word (append_dots broj_tocaka y))) $ mergers slogovi
    where
        slogovi = case (M.lookup (remove_dots xs) mapa) of
            Just x -> x
            Nothing -> []
        tocaka ys
            | null ys           = 0
            | last ys == '.'    = 1 + (tocaka $ init ys)
            | otherwise         = 0
        remove_dots ys
            | null ys           = []
            | last ys == '.'    = remove_dots $ init ys
            | otherwise         = ys
        append_dots n ys
            | n > 0     = append_dots (n-1) (ys ++ ".")
            | otherwise = ys
        broj_tocaka = tocaka xs

-- 5.h
lineBreaks :: HypMap -> Int -> Line -> [(Line,Line)]
lineBreaks mapa n xs = [breakLine n y | y <- podjele]
    where
        podjele = [(init xs) ++ [fst y] ++ [snd y] | y <- (hyphenate mapa (last xs))]

-- 5.i
insertions :: a -> [a] -> [[a]]
insertions needle haystack = [ (fst $ splitAt x haystack ) ++ [needle] ++ (snd $ splitAt x haystack) | x <- [0..(length haystack)]]

-- 5.j
insertBlanks :: Int -> Line -> [Line]
insertBlanks = undefined

-- 5.k
blankDistances :: Line -> [Int]
blankDistances = undefined

-- 5.l
avg :: Real a => [a] -> Double
avg xs
    | null xs   = 0
    | otherwise = (realToFrac $ sum xs) / (realToFrac $ length xs)

var xs = (var' 0 0 0 xs) / (realToFrac $ length xs - 1)
    where
      var' _ _ s [] = s
      var' m n s (x:xs) = var' nm (n + 1) (s + delta * (x - nm)) xs
         where
           delta = x - m
           nm = m + delta / (realToFrac $ n + 1)

-- 5.m
data Costs = Costs {
    blankCost :: Double,
    blankProxCost :: Double,
    blankUnevenCost :: Double,
    hypCost :: Double } deriving (Eq,Show)

defaultCosts = Costs {
    blankCost = 1,
    blankProxCost = 1,
    blankUnevenCost = 0.5,
    hypCost = 0.5 }

lineBadness :: Costs -> Line -> Double
lineBadness = undefined

-- 5.n
bestLineBreak :: Costs -> HypMap -> Int -> Line -> Maybe (Line,Line)
bestLineBreak = undefined

-- 5.o
justifyLine :: Costs -> HypMap -> Int -> Line -> [Line]
justifyLine = undefined

-- 5.p
justifyString :: String -> [String]
justifyString = undefined


