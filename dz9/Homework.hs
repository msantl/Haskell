import Data.List
import Data.Char
import Prelude
import System.IO
import Control.Monad
import qualified Data.Map as M
import Data.Time.Clock.POSIX
import Data.Text.Lazy

-- 1
getTime :: IO Double
getTime = realToFrac `fmap` getPOSIXTime

-- 1.1
time :: IO () -> IO Double
time op = do
    start <- getTime
    op
    end <- getTime
    return $ end - start

-- 2

-- 2.a
grep :: String -> FilePath -> IO ()
grep = undefined

-- 2.b
grepWithArgs :: IO ()
grepWithArgs = undefined

-- 2.c
grepText :: Data.Text.Lazy.Text -> FilePath -> IO ()
grepText = undefined

-- 3
type Table = (FilePath,[String])

-- 3.a
dbCreateTable :: String -> [String] -> IO Table
dbCreateTable = undefined

-- 3.b
dbDeleteTable :: Table -> IO ()
dbDeleteTable = undefined

-- 3.c
dbInsert :: Table -> [String] -> IO ()
dbInsert = undefined

-- 3.d
dbSelect :: Table -> ([String] -> Bool) -> IO [[String]]
dbSelect = undefined

-- 3.e
dbUpdate :: Table -> ([String] -> Bool) -> ([String] -> [String]) -> IO ()
dbUpdate = undefined

-- 3.f
dbPrintTable :: Table -> IO ()
dbPrintTable = undefined

