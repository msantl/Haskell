{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

import Prelude hiding (foldr,foldl,foldr1, all, elem, zipWith, sum)
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
data ListVector = LV [Double]

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
data SparseVector = SV (M.Map Int Double)

instance Vector SparseVector where
    empty = SV (M.empty)

    zipWith = undefined

    fromList = undefined
    toList   = undefined

    fromAssocList = undefined
    toAssocList = undefined

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

