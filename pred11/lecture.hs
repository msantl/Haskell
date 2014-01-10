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

-- 2.2
-- copyLines :: [Int] -> FilePath -> FilePath -> IO ()
-- copyLines xs src dest = withFile src ReadMode $ \h -> do


-- main function
main = numberLines
