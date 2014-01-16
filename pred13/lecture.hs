import Data.List
import Data.Char
import Control.Monad
import Data.Maybe
import System.Random


-- Example 1 - label nodes of a tree

data Tree a = Leaf a | Branch (Tree a) (Tree a) deriving (Show, Eq)

t1 = Branch (Branch (Leaf 'b') (Leaf 'c')) (Branch (Leaf 'd') (Leaf 'a'))

label :: Tree a -> Tree Int
label = snd . step 0
    where
        step n (Leaf _) = (n+1, Leaf n)
        step n (Branch t1 t2) = let
            (n1, t11) = step n t1
            (n2, t22) = step n1 t2
            in (n2, Branch t11 t22)

-- Example 2 - random number generator

g = mkStdGen 13
(r1, g2) = random g :: (Int, StdGen)
(r2, g3) = random g2 :: (Int, StdGen)
(r3, g4) = random g3 :: (Int, StdGen)

-- with state monads

data SM s a = SM (s -> (s, a))

instance Monad (SM s) where
    -- gives us a funciton that takes a state and returns the unaltered state
    -- together with a return value a
    return a = SM (\s -> (s, a))

    -- >>= :: m a -> (a -> m b) -> m b
    SM sm0 >>= fsm1 = SM $ \s0 ->
        let (s1, a1) = sm0 s0   -- left computation on the state
            SM sm1 = fsm1 a1    -- the computation on the "right monad"
            (s2, a2) = sm1 s1   -- right computation on the state
        in (s2, a2)

-- An example

inc :: SM Int ()
inc = SM (\s -> (s+1, ()))

dec :: SM Int ()
dec = SM (\s -> (s-1, ()))

foo :: SM Int()
foo = inc >> inc >> inc

runSM' :: SM s a -> s -> (s, a)
runSM' (SM sm0) s0 = sm0 s0

-- running state monad

v1 = runSM' foo 10
v2 = runSM' (dec >> inc >> dec) 0

-- returning only the result without the state
runSM :: SM s a -> s -> a
runSM (SM sm0) s0 = snd $ sm0 s0

v3 = runSM foo 10

-- transfering state to return value
get :: SM s s
get = SM (\s -> (s, s))

v4 = runSM (foo >> get) 10

-- defining the set function
set :: s -> SM s ()
set x = SM $ (\_ -> (x, ()))

v5 = runSM (set 5) 10
v6 = runSM (set 5 >> get) 10
v7 = runSM (set 5 >> inc >> get) 10
v8 = runSM (get >>= set) 10
v9 = runSM (get >>= set >> get) 10
v10 = runSM (return 0 >> inc >> get) 10
v11 = runSM (get >> return 5) 10
v12 = runSM (inc >> return 0) 10
v13 = runSM (dec >> return 0 >> get) 10
v14 = runSM (get >>= set . (+5) >> get) 10

foo2 :: SM Int Int
foo2 = do
    x <- get
    set (x+5)
    get

v15 = runSM foo2 7

foo3 :: SM Int ()
foo3 = do
    x <- get
    set (x+100)
    return ()

-- labeling the nodes of a tree - state monads
label2 :: Tree a -> SM Int (Tree Int)
label2 (Leaf _) = do
    n <- get
    set (n+1)
    return (Leaf n)

label2 (Branch t1 t2) = do
    t1' <- label2 t1
    t2' <- label2 t2
    return $ Branch t1' t2'

labelTree :: Tree a -> Tree Int
labelTree t = runSM (label2 t) 0

-- Exercise 1
nop1 :: SM s ()
nop1 = SM (\s -> (s, ()))

nop2 :: SM s ()
nop2 = get >>= set

nop3 :: SM s ()
nop3 = do
    return ()

reset1 :: SM Int ()
reset1 = undefined

reset2 :: SM Int ()
reset2 = undefined

reset3 :: SM Int ()
reset3 = undefined

update1 :: (s -> s) -> SM s ()
update1 = undefined

update2 :: (s -> s) -> SM s ()
update2 = undefined

update3 :: (s -> s) -> SM s ()
update3 = undefined

-- another example
tuples = do
    n <- [1..10]
    c <- "abc"
    return (n, c)

fooo [] = [[]]
fooo xs = do
    x <- xs
    ys <- fooo (delete x xs)
    return (x:ys)


