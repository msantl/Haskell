import qualified Data.Map as M
import Data.List
import Data.Char
import Prelude

-- 1
data Date = Date Int Int Int deriving Show

data Animal = Animal {
    species     :: String,
    name        :: String,
    legNum      :: Maybe Int,
    birthday    :: Date,
    dangerLvl   :: Int } deriving Show

testDog = Animal "Dog" "Fluffy" (Just 4) (Date 16 3 1975) 10

-- 1.a
avgLegNum :: [Animal] -> Double
avgLegNum xs = (fromIntegral brojnik) / (fromIntegral nazivnik)
    where
        brojnik   = foldl (\acc x -> acc + x) 0 $ map numberOf xs
        nazivnik  = length xs
        numberOf x = case legNum x of
            Just p -> p
            Nothing -> 0

-- 1.b
canDrinkBeer :: [Animal] -> [String]
canDrinkBeer xs = canDrinkBeer' xs []
    where
        canDrinkBeer' [] res     = res
        canDrinkBeer' ((Animal _ name _ (Date _ _ year) _):xs) res
            | year <= 1995  = canDrinkBeer' xs (name : res)
            | otherwise     = canDrinkBeer' xs res

-- 1.c
getFakeID :: Animal -> Int -> Animal
getFakeID x@(Animal _ _ _ (Date day month year) _) n = x {birthday = (Date day month (year - n))}

-- 2
data BinaryTree a = Null | Node a (BinaryTree a) (BinaryTree a) deriving (Show, Eq)

testTree = Node 1 (Node 2 (Node (-4) Null Null) (Node 3 Null Null)) (Node 2 (Node 1 Null (Node 10 Null (Node (-2) Null Null))) Null)

-- 2.a
numNodes :: BinaryTree a -> Int
numNodes Null = 0
numNodes (Node a l r) = (numNodes l) + (numNodes r) + 1

-- 2.b
averageNodeDegree :: (Num a, Integral a) => BinaryTree a -> Double
averageNodeDegree Null = 0
averageNodeDegree tree = (fromIntegral $ sumDegree tree) / (fromIntegral $ numNodes tree)
    where
        sumDegree Null = 0
        sumDegree (Node a l r) = (sumDegree l) + (sumDegree r) + curr
            where
                curr = left + rajt
                left = if l /= Null then 1 else 0
                rajt = if r /= Null then 1 else 0

-- 2.c
treeDepth :: BinaryTree a -> Int
treeDepth Null = 0
treeDepth (Node a l r) = (max ((treeDepth l) + 1) ((treeDepth r) + 1) )

-- 2.d
preorder :: BinaryTree a -> [a]
preorder Null = []
preorder (Node a l r) = [a] ++ (preorder l)  ++ (preorder r)

-- 2.e
inorder :: BinaryTree a -> [a]
inorder Null = []
inorder (Node a l r) = (inorder l) ++ [a] ++ (inorder r)

-- 2.f
postorder :: BinaryTree a -> [a]
postorder Null = []
postorder (Node a l r) = (postorder l) ++ (postorder r) ++ [a]

-- 3
listLength :: BList a -> Int
listLength (Empty _) = 0
listLength (ListNode a next) = 1 + (listLength next)

listSize :: BList a -> Int
listSize (Empty n) = n
listSize (ListNode a next) = listSize next

-- 3.a
data BList a = Empty Limit | ListNode a (BList a) deriving Show
type Limit = Int

-- 3.b
empty :: Limit -> BList a
empty n = Empty n

-- 3.c
fromList :: [a] -> BList a
fromList xs = fromList' xs 0
    where
        fromList' [] n = Empty n
        fromList' (x:xs) n = ListNode x (fromList' xs (n+1))

-- 3.d
limited :: Limit -> BList a -> BList a
limited n xs = limited' xs 0 n
    where
        limited' (Empty _) _ n = Empty n
        limited' (ListNode x next) m n
            | m < n = ListNode x (limited' next (m+1) n)
            | otherwise = Empty n

-- 3.e
cons :: a -> BList a -> BList a
cons x xs
    | (listLength xs) < (listSize xs) = (ListNode x xs)
    | otherwise = error "too many elements!"

-- 3.f
concat' :: [BList a] -> BList a
concat' [] = Empty 0
concat' (x:xs) = concatenate x xs 0
    where
        concatenate (Empty k) [] l = Empty (l + k)
        concatenate (Empty k) (x:xs) l = concatenate x xs (l+k)
        concatenate (ListNode a next) xs l = (ListNode a (concatenate next xs l))

-- 4
e = Add (Var "x") (Mul (Val 2.0) (Add (Var "y") (Val 7.5)))
vars = M.fromList[("x", 10), ("y", 2.5)]

-- 4.a
data Expr = Var String | Val Double | Add (Expr) (Expr) | Sub (Expr) (Expr) | Mul (Expr) (Expr) | Div (Expr) (Expr)
    deriving Show

-- 4.b
showExpr :: Expr -> String
showExpr (Var xs) = xs
showExpr (Val x)
    | x < 0 = "(" ++ show x ++ ")"
    | otherwise = show x
showExpr (Add a b) = (showExpr a) ++ "+" ++ (showExpr b)
showExpr (Sub a b) = (showExpr a) ++ "-" ++ (showExpr b)
showExpr (Div a b) = (showExpr a) ++ "/(" ++ (showExpr b) ++ ")"
showExpr (Mul a b) = (showExpr a) ++ "*(" ++ (showExpr b) ++ ")"

-- 4.c
subst :: String -> Expr -> Expr -> Expr

subst xs sub (Add a b) = Add (subst xs sub a) (subst xs sub b)
subst xs sub (Sub a b) = Sub (subst xs sub a) (subst xs sub b)
subst xs sub (Div a b) = Div (subst xs sub a) (subst xs sub b)
subst xs sub (Mul a b) = Mul (subst xs sub a) (subst xs sub b)

subst xs sub (Var ys)
    | xs == ys = sub
    | otherwise = Var ys

subst _ _ (Val ys) = (Val ys)

-- 4.d
type VarAssignments = M.Map String Double
eval :: VarAssignments -> Expr -> Maybe Double

eval mapa (Add a b) = case (l, r) of
        (Just x, Just y)    -> Just $ x + y
        (_, _)              -> Nothing
    where
        l = eval mapa a
        r = eval mapa b

eval mapa (Sub a b) = case (l, r) of
        (Just x, Just y)    -> Just $ x - y
        (_, _)              -> Nothing
    where
        l = eval mapa a
        r = eval mapa b

eval mapa (Mul a b) = case (l, r) of
        (Just x, Just y)    -> Just $ x * y
        (_, _)              -> Nothing
    where
        l = eval mapa a
        r = eval mapa b

eval mapa (Div a b) = case (l, r) of
        (Just x, Just y)    -> Just $ x / y
        (_, _)              -> Nothing
    where
        l = eval mapa a
        r = eval mapa b

eval mapa (Var x)
    | M.member x mapa   = Just $ (mapa M.! x)
    | otherwise         = Nothing

eval mapa (Val x) = Just x

-- 5
data Trie k a = Leaf a | Branch [(Maybe k, Trie k a)] deriving Show

-- 5.a
empty' :: Trie k a
empty' = Branch []

-- 5.b
insert' :: Eq k => [k] -> a -> Trie k a -> Trie k a

insert' []      id _               = Branch [(Nothing, Leaf id)]

insert' a@(x:xs) id (Branch (l:ls))  = case fst l of
    Just y ->
        if x == y
        then    Branch ([(Just x, insert' xs id (snd l))] ++ ls)
        else    Branch (l : branchToList (insert' a id (Branch ls)))
    Nothing ->  Branch (l : branchToList (insert' a id (Branch ls)))
    where
        branchToList (Branch x) = x
        branchToList _          = []

insert' (x:xs)  id (Branch [])      = Branch [(Just x, insert' xs id (Branch []))]

-- 5.c
fromList' :: Eq k => [([k], a)] -> Trie k a
fromList' = foldl (\acc x -> insert' (fst x) (snd x) acc) empty'

-- 5.d
lookup' :: Eq k => [k] -> Trie k a -> Maybe a

lookup' _           (Branch [])         = Nothing

lookup' []          (Branch (l:ls))     = case fst l of
    Just y  -> lookup' [] (Branch ls)
    Nothing -> getValue $ snd l
    where
        getValue (Leaf x)   = Just x
        getValue _          = Nothing

lookup' a@(x:xs)    (Branch (l:ls))     = case fst l of
    Just y  -> if x == y then (lookup' xs (snd l)) else (lookup' a (Branch ls))
    Nothing -> Nothing

-- 5.e
delete' :: Eq k => [k] -> Trie k a -> Trie k a

delete' [] (Branch (l:ls)) = case fst l of
    Just y -> delete' [] (Branch ls)
    Nothing -> (Branch ls)

delete' a@(x:xs) (Branch (l:ls))  = case fst l of
    Just y ->
        if x == y
        then    let sub = delete' xs (snd l)
                in Branch ( (if null $ branchToList sub then [] else [(Just x, sub)]) ++ ls)
        else    Branch (l : branchToList (delete' a (Branch ls)))
    Nothing ->  Branch (l : branchToList (delete' a (Branch ls)))
    where
        branchToList (Branch x) = x
        branchToList _          = []
