import System.Environment
import Data.Char

expandList :: [Int] -> Int -> [Int]
expandList xs n = 

dname :: [Int] -> Int -> [Int]
dname xs n = drop n . take 10 $ expandList xs (n +10)

main :: IO ()
main = do
    args    <- getArgs
    content <- readFile (args !! 0)
    let n = read (lines content !! 0) :: Int
    print $ dname [3,7] n
