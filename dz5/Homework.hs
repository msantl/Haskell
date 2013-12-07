import Data.Char
import Data.List
import Data.List.Split
import Prelude

-- 1
concatMap' :: (a -> [b]) -> [a] -> [b]
concatMap' f xs = concat' [f x | x <- xs]
    where
        concat' [] = []
        concat' (y:ys) = y ++ concat' ys

-- 2
reduce :: (a -> b -> a) -> a -> [b] -> a
reduce f x0 [] = x0
reduce f x0 [x] = f x0 x
reduce f x0 (x:xs) = reduce f (f x0 x) xs

-- 3
reduce1 :: (a -> a -> a) -> [a] -> a
reduce1 f [] = error "Got an empty list"
reduce1 f [x] = x
reduce1 f (x:xs) = reduce f x xs

-- 4
type Vertex = Integer
type Graph = [(Vertex, Vertex)]
type Queue = [Vertex]
type Map = [(Vertex, Integer)]

-- 4.a
popFromQ :: Queue -> (Vertex, Queue)
popFromQ [] = error "Empty queue"
popFromQ (q:qs) = (q, qs)

-- 4.b
pushToQ :: Queue -> Vertex -> Queue
pushToQ q v = q ++ [v]

-- 4.c
isEmptyQ :: Queue -> Bool
isEmptyQ q = null q

-- 4.d
isInQ :: Queue -> Vertex -> Bool
isInQ q v = elem v q

-- 4.e
getQSingleton :: Vertex -> Queue
getQSingleton v = [v]

-- 4.f
getValFor :: Map -> Vertex -> Integer
getValFor [] v = error "Key not found"
getValFor (m:ms) v
    | fst m == v = snd m
    | otherwise = getValFor ms v

-- 4.g
putKeyVal :: Map -> Vertex -> Integer -> Map
putKeyVal [] v i = [(v, i)]
putKeyVal (m:ms) v i
    | fst m == v = [(v, i)] ++ ms
    | otherwise = m : putKeyVal ms v i

-- 4.h
isKeyInMap :: Map -> Vertex -> Bool
isKeyInMap m v = or $ map (\(key, value) -> key == v ) m

-- 4.i
getMapSingleton :: Vertex -> Integer -> Map
getMapSingleton v i = [(v, i)]

-- 4.j
--
-- shortestPath - returns the shortest path between two nodes
--
-- this function is just a wrapper function for the bfs function which performs
-- the bfs algorithm
shortestPath :: Graph -> Vertex -> Vertex -> Integer
shortestPath g v1 v2
    | v1 == v2 = 0
    | otherwise = bfs v2 (getQSingleton v1) (getMapSingleton v1 0) g

bfs :: Vertex -> Queue -> Map -> Graph -> Integer
bfs goal queue dist g
    | isEmptyQ queue = -1
    | isInQ queue goal = getValFor dist goal
    | otherwise = bfs goal (updateQueue queue' neighbours) (updateMap dist neighbours) g
        where
            (curr, queue') = popFromQ queue
            neighbours = [snd x | x <- g, fst x == curr]
            updateQueue q [] = q
            updateQueue q (nd:nds)
                | not $ isInQ queue nd = (getQSingleton nd) ++ (updateQueue q nds)
                | otherwise = updateQueue q nds
            updateMap m [] = m
            updateMap m (nd:nds)
                | not $ isKeyInMap dist nd = (getMapSingleton nd ((getValFor dist curr) + 1)) ++ (updateMap m nds)
                | (getValFor dist curr) + 1 < (getValFor dist nd) = (getMapSingleton nd ((getValFor dist curr) + 1)) ++ (updateMap m nds)
                | otherwise = updateMap m nds

-- 5
type FileSystem = [(FilePath, [FilePath])]
type FileSystemState = (FilePath, FileSystem)

testFS = [("/", ["dir1/", "homework.hs"]), ("/dir1/", ["dir2/"]), ("/dir1/dir2/", [])]

absoulute path = (path !! 0) == '/'

-- 5.a
pwd :: FileSystemState -> String
pwd fss = fst fss

-- 5.b
ls :: FileSystemState -> String
ls fss = unwords [unwords $ snd x | x <- (snd fss), fst fss == fst x]

-- 5.c
buildAbsPath :: String -> String -> String
buildAbsPath dir path
        | absoulute path = path
        | last res == '/' && length res > 1 = init res
        | otherwise = res
    where
        res = concat $ build (init $ transform dir) (transform path)
        transform = map (\x -> if elem '.' x then x else x ++ "/") . splitOn "/"
        build [] xs = build ["/"] xs
        build wd [] = wd
        build wd (x:xs)
            | x == "." = build wd xs
            | x == ".." = build (init wd) xs
            | otherwise = build (wd ++ [x]) xs

-- 5.d
cd :: FileSystemState -> String -> FileSystemState
cd state path
    | invalid new_path = error "No such directory"
    | otherwise = (new_path, snd state)
    where
        new_path = if x == "//" then "/" else x
            where x = new_path_helper
        new_path_helper
            | absoulute path = path ++ "/"
            | otherwise = (buildAbsPath (fst state) path) ++ "/"
        invalid xs = not $ or [ xs== fst x | x <- snd $ state ]

-- 5.e
rm :: FileSystemState -> String -> FileSystemState
rm state path
    | path == "." || fst state == path = error "Can not remove current directory"
    | path == ".." = error "Can not remove parent directory"
    | invalid new_path = error "There is no such file/directory"
    | otherwise = (fst state, file_system)
    where
        file_system = [ (fst x, [ y | y <- snd x, (fst x) ++ y /= new_path] ) | x <- remove]
        remove = filter (\(x,y) -> not $ isPrefixOf new_path x) (snd $ state)
        new_path
            | elem '.' new_path_helper = init x
            | otherwise = if x == "//" then "/" else x
            where x = new_path_helper
        new_path_helper
            | absoulute path = path ++ "/"
            | otherwise = (buildAbsPath (fst state) path) ++ "/"
        invalid xs = not $ or [ (fst x) ++ y == new_path | x <- snd state, y <- snd x]

