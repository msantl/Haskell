University of Zagreb
Faculty of Electrical Engineering and Computing

PROGRAMMING IN HASKELL

Academic Year 2013/2014

LECTURE 11: Input/output operations 2

v1.0

(c) 2013 Jan Šnajder

==============================================================================

> import Data.Char
> import Data.List
> import qualified Data.Map as M
> import Control.Monad
> import System.IO
> import System.Directory
> import System.IO.Error
> import System.Environment
> import System.FilePath
> import System.Random
> import Control.Exception

== READING FROM STREAMS ======================================================

Up until now we read from the standard input line by line ('getLine' function).
We can also read character by character ('getChar' function). For example, a
program that reads in characters from standard input, converts to upper case,
and outputs them to standard output:

> main1 :: IO ()
> main1 = do
>   c <- getChar
>   putChar $ toUpper c
>   eof <- isEOF
>   if eof then return () else main1

We can do the same line by line:

> main2 :: IO ()
> main2 = do
>   l <- getLine
>   putStrLn $ map toUpper l
>   eof <- isEOF
>   if eof then return () else main2

In situations like these, when we eventually want to read in all the data from
standard input (or a file), it is better to work with STREAMS right away. A
stream is actually a string that contains the whole input. But, because Haskell
is lazy, the whole string won't be read at once. It is only when new elements
of the stream (characters or lines) are required that these will actually be
read into a string. On the other hand, when an element of the stream (a
character or a line) is no longer needed, it will be removed from the memory by
the garbage collector.

For reading streams from standard input we use the function:

  getContents :: IO String

For instance, a program that reads in a text from standard input and converts
all letters into uppercase:

> main3 :: IO ()
> main3 = do
>   s <- getContents
>   putStr $ map toUpper s

This will read in at most 10 characters:

> main4 :: IO ()
> main4 = do
>   s <- getContents
>   putStr . take 10 $ map toUpper s

And this at most 10 lines:

> main5 :: IO ()
> main5 = do
>   s <- getContents
>   putStr . unlines . take 10 . lines $ map toUpper s

A function that reads lines from standard input and outputs all non-empty
lines:

> main6 :: IO ()
> main6 = do
>   s <- getContents
>   putStr . unlines . filter (not . null) $ lines s

Often we need to read some data from standard input, transform them, and print
to standard output. We can accomplish that succinctly using 'interact':

  interact :: (String -> String) -> IO ()
  interact f = do s <- getContents
                  putStr (f s)

For example, we could have defined the above functions like this:

> main7 :: IO ()
> main7 = interact (map toUpper)

> main8 :: IO ()
> main8 = interact (unlines . filter (not . null) . lines)

== EXERCISE 1 ================================================================

1.1.
- Define a function that removes from standard input every second line and
  prints the result to standard output.
    filterOdd :: IO ()

1.2.
- Define a function that prefixes each line from standard input with a line
  number (number + space).
    numberLines :: IO ()

1.3.
- Define a function to remove from standard input all words from a given set of
  words.
    filterWords :: Set String -> IO ()

== WORKING WITH FILES ========================================================

We've been using function from 'System.IO' that work with standard input and
output. This module also provides similar functions to work with files:

  hPutStr :: Handle -> String -> IO ()
  hPutStrLn :: Handle -> String -> IO ()
  hGetLine :: Handle -> IO String
  hGetChar :: Handle -> IO Char
  hGetContents :: Handle -> IO String

These function take a file handle (a value of type 'Handle') that contains all
the relevant OS information about the file. We get a handle by opening a file:

  openFile :: FilePath -> IOMode -> IO Handle

'FilePath' is a synonym for 'String'. 'IOMode' is:

  data IOMode =  ReadMode | WriteMode | AppendMode | ReadWriteMode

After we're finished with the file, we need to close it:

  hClose :: Handle -> IO ()

E.g., a function that opens a given file and prints its content:

> cat1 :: FilePath -> IO ()
> cat1 f = do
>   h <- openFile f ReadMode
>   printLines h
>   hClose h
>
> printLines :: Handle -> IO () 
> printLines h = do 
>   l <- hGetLine h
>   putStrLn l
>   eof <- hIsEOF h
>   if eof then return () else printLines h

A genuine Haskeller will write this in a neater way using streams:

> cat2 :: FilePath -> IO ()
> cat2 f = do
>   h <- openFile f ReadMode
>   s <- hGetContents h
>   putStr s
>   hClose h

Let's ginger it with line numbers:

> cat3 :: String -> IO ()
> cat3 f = do
>   h <- openFile f ReadMode
>   s <- hGetContents h
>   forM_ (zip [0..] (lines s)) $ \(i,l) -> 
>     putStrLn $ show i ++ ": " ++ l
>   hClose h

or

> cat4 :: String -> IO ()
> cat4 f = do
>   h <- openFile f ReadMode
>   s <- hGetContents h
>   putStr . unlines . zipWith (\i l -> show i ++ ": " ++ l) [0..] $ lines s
>   hClose h

This situation (opening a file, processing it, and closing it) is quite common,
hence there's a special function that makes this easier:

  withFile :: FilePath -> IOMode -> (Handle -> IO a) -> IO a

This function also takes care that the file is closed if an error occurs.

Printing the content of a file with line numbers:

> cat5 :: String -> IO ()
> cat5 f = withFile f ReadMode $ \h -> do
>  s <- hGetContents h
>  putStr . unlines . zipWith (\i l -> show i ++ ": " ++ l) [0..] $ lines s

For standard input and output there are predefined handles (which need not be
opened nor closed): 'stdin', 'stdout' i 'stderr'.

IO function on standard input/output are actually defined as follows:

  getLine = hGetLine stdin
  putStr = hPutStr stdout
  putStrLn = hPutStrLn stdout
  getContents = hGetContents stdin 
  ...

A couple of other useful functions from 'System.IO':

  hFileSize :: Handle -> IO Integer
  hSeek :: Handle -> SeekMode -> Integer -> IO ()
  hTell :: Handle -> IO Integer
  hFlush :: Handle -> IO ()
  hIsEOF :: Handle -> IO Bool

== EXERCISE 2 ================================================================

2.1.
- Define a function
  wc :: FilePath -> IO (Int, Int, Int)
  that counts the number of characters, words, and lines in a file.

2.2. 
- Define a function
  copyLines :: [Int] -> FilePath -> FilePath -> IO ()
  that copies given lines from the first file into the second.

==============================================================================

Reading from a stream (using 'hGetContents') and writing to it (using
'hPutStr') is a common pattern, hence there are functions that make this
easier, without the need to explicitly open a file and maintain a handle:

  readFile  :: FilePath -> IO String
  writeFile :: FilePath -> String -> IO ()

> cat6 :: String -> IO ()
> cat6 f = do
>  s <- readFile f
>  putStr . unlines . zipWith (\i l -> show i ++ ": " ++ l) [0..] $ lines s

> uppercaseFile :: FilePath -> IO ()
> uppercaseFile f = do
>   s <- readFile f
>   putStr $ map toUpper s

> interlaceFiles :: FilePath -> FilePath -> FilePath -> IO ()
> interlaceFiles f1 f2 f3 = do
>   s1 <- readFile f1
>   s2 <- readFile f2
>   writeFile f3 . unlines $ interlace (lines s1) (lines s2)
>   where interlace xs ys = concat $ zipWith (\x1 x2 -> [x1,x2]) xs ys

Serialization of a data structure (dumping it to a disk) and deserialization
(reading it from a disk) can be accomplished in a straightforward manner using
'readFile' and 'writeFile', respectively, in a combination with 'read' and
'show', respectively. For example, serialization of a list:

> main9 :: IO ()
> main9 = do 
>   let l = [(x,y) | x <- [0..100], y <- [x..100]]
>   writeFile "list.txt" $ show l

Deserialization:

> main10 :: IO ()
> main10 = do
>   s <- readFile "list.txt" 
>   let l = read s :: [(Int,Int)]
>   print l

As always when we use 'read', we must specify the type of the value to be read
and we must take care that whatever is in the string really parses accordingly
(otherwise we get a "no parse" error).

NOTE: The examples above do textual serialization, which is inefficient for
large data structures. For binary serialization, use of of the binary
serialization packages, such as binary, cereal, or beamable:
http://hackage.haskell.org/package/binary
http://hackage.haskell.org/package/cereal-plus
http://hackage.haskell.org/package/beamable

Example: we maintain a word translation dictionary, implemented as 'Data.Map',
which we store in a file. We make queries to the dictionary via the keyboard.
For words which don't exists in the dictionary, we can provide translations
that are added to the dictionary and stored before exiting.

> type Dict = M.Map String String
> dictFile = "dict.txt"

> main11 :: IO ()
> main11 = do
>   d1 <- readDict
>   d2 <- useDict d1
>   writeFile dictFile $ show d2

> readDict :: IO Dict
> readDict = do
>   e <- doesFileExist dictFile
>   if e then do
>     s <- readFile dictFile
>     return $ read s 
>   else return M.empty

> useDict :: Dict -> IO Dict
> useDict d = do
>   putStrLn "Enter term: "
>   w1 <- getLine
>   if null w1 then return d else
>     case M.lookup w1 d of
>       Just w2 -> do
>         putStrLn w2
>         useDict d
>       Nothing -> do
>         putStrLn $ "No entry. How would you translate " ++ w1 ++ "?"
>         w2 <- getLine 
>         useDict $ M.insert w1 w2 d

REMARK: It is generally a bad idea to open a file using 'readFile' and then
write to the same file using 'writeFile'. This, however, is possible only if
the whole stream is consumed before writing to the file. This is indeed the
case in the above example, as the 'lookup' function forces the whole string to
be read in from the file (why?).

In contrast, this won't work:

> main12 :: IO ()
> main12 = do
>   d1 <- readDict
>   writeFile "dict.txt" $ show d1

Occasionally we need to be able to create a temporary file, for example when
changing an existing file. Use the following function for this:

  openTempFile :: FilePath -> String -> IO (FilePath, Handle)

The function takes a path and a file name, and returns a unique name of a newly
created temporary file and its handle. After using the file, you have to close
it using 'hClose'.

E.g., a function that sorts alphabetically the lines in a given file:

> sortFile :: FilePath -> IO ()
> sortFile f = do
>   (ft,ht) <- openTempFile "" f
>   s <- readFile f
>   hPutStr ht . unlines . sort $ lines s
>   hClose ht
>   renameFile ft f

REMARK: In most cases it is better to write the altered content to the standard
output than to change the original file. This enables the user to check the
output and do with it what she wants.

== EXERCISE 3 =================================================================

3.1.
- Define a function
    wordTypes :: FilePath -> IO Int
  to compute the number of distinct words in the given file.

3.2.
- Define a function 
    diff :: FilePath -> FilePath -> IO ()
  that takes two file names, compares their corresponding lines, and then
  outputs to standard output all lines in which the files differ. Lines should 
  be printed one below the other, prefixed with "<" for the first and ">" for
  the second file.

3.3.
- Define a function
    removeSpaces :: FilePath -> IO () 
  that removes trailing spaces from all lines in the given file.
  The function should change the original file.

== EXCEPTION HANDLING =========================================================

IO actions can cause exceptions. Exceptions can be handled but only within an
IO monad. There are no specific syntax constructs for handling exceptions.
Instead they are handled using functions from the 'Control.Exception':

  try :: Exception e => IO a -> IO (Either e a)
  catch :: Exception e => IO a -> (e -> IO a) -> IO a

NOTE: Functions for exception handing specifically for IO operations reside in
the 'System.IO.Error' module. However, we'll be using 'Control.Exception'
because its more general in that it can be used to catch other kinds of
exceptions (pure code exceptions and asynchronous exceptions, such as stack
overflow). Note, however, that even these other kind of exceptions can only be
caught within an IO monad.

'try' takes an action and returns 'IO (Right a)' if everything turned out ok,
otherwise it returns 'Left e' if an exception occurred.

For example:

> cat7 :: String -> IO ()
> cat7 f = do
>  r <- try $ cat6 f
>  case r of 
>    Left e  -> putStrLn $ "Error: " ++ ioeGetErrorString e
>    _       -> return ()

'catch' takes two actions: one that can cause an exception and another that
handles it. For example:

> cat8 :: String -> IO ()
> cat8 f = catch (cat6 f) $ \e ->
>   if isDoesNotExistError e then putStrLn "Error: someone stole the file"
>   else ioError e  -- rethrowing the exception

'try' and 'catch' functions take care that the program recovers from an
exception. If we only want to clean up after an exception occurred (e.g., close
the open files), we use the following function from 'Control.Exception':

  bracket :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c

The function 'bracket' takes three actions: action 'IO a' acquires a resource,
action 'a -> IO b' releases it, while action 'a -> IO c' actually does some
processing with the resource. If an exception occurs, the action that releases
the resource will be executed, and the exception will be rethrown. A typical
use of this pattern is as follows:

 bracket (openFile f ReadMode) hClose (\h-> do ...)

The 'withFile' function, which we introduced earlier, is actually defined as:

  withFile f mode = bracket (openFile f mode) hClose

For example, you might define a 'withTempFile' function as this:

> withTempFile :: FilePath -> String -> ((FilePath, Handle) -> IO c) -> IO c
> withTempFile path f = bracket
>   (openTempFile path f)
>   (\(f,h) -> do hClose h; removeFile f)

== ENVIRONMENT VARIABLES ======================================================

Arguments can be passed to a program via a command line. The following
functions from the 'System.Environment' module retrieve these arguments:

  getProgName :: IO String
  getArgs :: IO [String]
  
> main13 :: IO ()
> main13 = do
>   xs <- getArgs
>   x <- getProgName
>   putStrLn $ "Program " ++ x ++ " is invoked with arguments " ++ show xs

You can use the 'withArgs' function to supply arguments to this function, or
you can rename the function to 'main' and then run ':main' in ghci. Of course,
you can also build the program and run it from the command line.

  withArgs ["arg1","arg2"] main13
  :main arg1 arg2

Example: 'sort' that opens a file (if it exists, otherwise it takes the
standard input), sorts it, and prints to standard output:

> main14 :: IO ()
> main14 = do
>   xs <- getArgs
>   h <- case xs of
>     (f:_) -> do e <- doesFileExist f
>                 if e then openFile f ReadMode else return stdin
>     []    -> return stdin
>   s <- hGetContents h
>   putStr . unlines . sort $ lines s

REMARK: For a more sophisticated (unixoidal) command line argument processing you
should be using functions from the 'System.Console.GetOpt' module:
'System.Console.GetOpt'.
http://www.haskell.org/ghc/docs/latest/html/libraries/base/System-Console-GetOpt.html
or one of the modules that builds on top of it, such as:
http://hackage.haskell.org/package/parseargs

== EXERCISE 4 =================================================================

4.1.
- Define a function
    fileHead :: IO ()
  that prints the first 'n' lines from a file. The name of the file and the
  number of lines are specified at the command line, e.g.:
    filehead -5 input.txt
  If the number of lines is missing, default to 10. If file name is missing,
  read from the standard input. If the file doesn't exist, print an error
  message and exit with failure using 'exitFailure' from 'System.Exit'.

4.2.
- Define a function
    sortFiles :: IO ()
  that sorts lines from multiple files and prints them to standard output.
  File names are provided at the command line.
  "sortFiles file1.txt file2.txt file3.txt"
  If any of the files does not exist, print an error message.

== FILE SYSTEM OPERATIONS =====================================================

Module 'System.Directory' contains a set of functions for interacting with a
file system. The most important ones are:

  copyFile :: FilePath -> FilePath -> IO ()
  createDirectory :: FilePath -> IO ()
  doesDirectoryExist :: FilePath -> IO Bool
  doesFileExist :: FilePath -> IO Bool
  removeFile :: FilePath -> IO ()
  renameFile :: FilePath -> FilePath -> IO ()
  getDirectoryContents :: FilePath -> IO [FilePath]

Functions for operations on file paths reside in the 'System.FilePath' module.
The most important ones are:

  (</>) :: FilePath -> FilePath -> FilePath
  takeBaseName :: FilePath -> String
  takeDirectory :: FilePath -> FilePath
  takeExtension :: FilePath -> String
  takeFileName :: FilePath -> FilePath

== RANDOM NUMBER GENERATOR ===================================================

Module 'System.Random' provides functions for generating (pseudo)random numbers
within the IO monad.

The main (and most general) function is

  random :: (RandomGen g, Random a) => g -> (a, g)

'Random' is a type class for types whose values can be randomly generated,
while 'RandomGen' is a type class for the various implementations of random
number generators (actually a random generator API).

In most cases you will want to use the standard random number generator called
'StdGen', which is an instance of the 'RandomGen' class. To create a standard
random number generator, use the function

  mkStdGen :: Int -> StdGen

which takes a seed value and returns the standard RNG initialized with this
seed.

> g = mkStdGen 13
> (r1,g2) = random g :: (Int, StdGen)
> (r2,g3) = random g2 :: (Int, StdGen)
> (r3,g4) = random g3 :: (Int, StdGen)

  randoms :: (RandomGen g, Random a) => g -> [a]

> xs = randoms g :: [Float]
> fiveCoins = take 5 $ randoms g :: [Bool]

To generate random numbers from within an interval, use

  randomR :: (RandomGen g, Random a) => (a, a) -> g -> (a, g)
  randomRs :: (RandomGen g, Random a) => (a, a) -> g -> [a]

> fiveDice = take 5 $ randomRs (1,6) g :: [Int]

The obvious problem is that, because Haskell is pure, we need to drag around
the last instance of the RNG. Another problem is that we must specify the seed.
Both problems disappear if we use an RNG in the IO monad. To create an RNG in
the IO monad, use

  getStdGen :: IO StdGen

> main15 :: IO ()
> main15 = do
>   g <- getStdGen
>   putStrLn $ take 10 (randomRs ('a','z') g)
>   g2 <- getStdGen
>   putStrLn $ take 10 (randomRs ('a','z') g2)

The problem here is that we don't get different values every time we run the
RNG, despite calling 'getStdGen' twice. We need to "split" the RNG using
'newStdGen':

> main16 :: IO ()
> main16 = do
>   g <- getStdGen
>   putStrLn $ take 10 (randomRs ('a','z') g)
>   g2 <- newStdGen
>   putStrLn $ take 10 (randomRs ('a','z') g2)

A shorter way to accomplish the same:

  getStdRandom :: (StdGen -> (a, StdGen)) -> IO a

> main17 :: IO ()
> main17 = do
>   x <- getStdRandom (randomR (0,100)) :: IO Int
>   print x

NOTE: You can also use the 'Control.Monad.Random' module, which provides a more
flexible framework for random number generation:
http://hackage.haskell.org/package/MonadRandom

== EXERCISE 5 =================================================================

5.1.
- Define your own implementation of
    randoms' :: (RandomGen g, Random a) => g -> [a]

5.2.
- Define a function
    randomPositions :: Int -> Int -> Int -> Int -> IO [(Int,Int)]
  that returns a list of randomly generated integer coordinates from within a
  given interval.
    randomPositions 0 10 0 10 => [(2,1),(4,3),(7,7),...

