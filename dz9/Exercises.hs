import Data.List
import Data.Char
import Prelude
import System.IO
import Control.Monad
import qualified Data.Map as M

interact :: (String -> String) -> IO ()
interact f = do
    s <- getContents
    putStr(f s)

-- lecture 11

-- 1.1
filterOdd :: IO ()
filterOdd = do
    s <- getContents
    putStr . unlines $ map snd $ filter (odd . fst) $ zip [1..] (lines s)

-- 1.2
numberLines :: IO ()
numberLines = do
    s <- getContents
    putStr . unlines $ map f $ zip [1..] (lines s)
    where
        f (x, y) = (show x) ++ " " ++ y


-- 2.1
wc :: FilePath -> IO (Int, Int, Int)
wc f = withFile f ReadMode $ \h -> do
    s <- hGetContents h
    return $ (length s, length $ words s, length $ lines s)

-- lecture 13

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

-- transfering state to return value
get :: SM s s
get = SM (\s -> (s, s))


-- defining the set function
set :: s -> SM s ()
set x = SM $ (\_ -> (x, ()))


-- 1.1
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


