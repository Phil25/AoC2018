import           System.Environment
import           Data.List
import qualified Data.Map as Map

--- Part One ---
getFreq :: String -> [Int]
getFreq xs = Map.elems $ Map.fromListWith (+) [(x, 1) | x <- xs]

countExacts :: (Int, Int) -> String -> (Int, Int)
countExacts (ex2, ex3) val
    | has2 && has3 = (ex2+1, ex3+1)
    | has2         = (ex2+1, ex3)
    | has3         = (ex2, ex3+1)
    | otherwise    = (ex2, ex3)
        where frequency = getFreq val
              has2 = 2 `elem` frequency
              has3 = 3 `elem` frequency

getChecksum :: [String] -> Int
getChecksum contents = ex2 * ex3
    where (ex2, ex3) = foldl countExacts (0, 0) contents

--- Part Two ---
getDiff :: String -> String -> Int
getDiff (x:[]) (y:_) = 0
getDiff (x:xs) (y:ys)
    | x /= y    = getDiff xs ys + 1
    | otherwise = getDiff xs ys

getOneCharDiff :: [(String, String)] -> (String, String)
getOneCharDiff (_:[]) = ("", "")
getOneCharDiff ((s1,s2):xs)
    | getDiff s1 s2 == 1 = (s1, s2)
    | otherwise          = getOneCharDiff xs

main :: IO ()
main = do
    args    <- getArgs
    content <- readFile (args !! 0)
    let testset = lines content
        combs   = [(x,y) | (x:rest) <- tails testset, y <- rest] -- combinations w/o repetitions
        result  = getOneCharDiff $ combs
    print $ getChecksum testset
    print $ fst result
    print $ snd result
