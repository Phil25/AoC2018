import           System.Environment
import qualified Data.Set as Set

--- Part One ---
readFreq :: String -> Int
readFreq ('-':xs) = negate $ read xs
readFreq (_:xs) = read xs

getFreq :: String -> [Int]
getFreq = map readFreq . lines

--- Part Two ---
findDup :: [Int] -> (Set.Set Int) -> Int -> Int
findDup (x:xs) freqSet acc
    | acc `Set.member` freqSet = acc
    | otherwise                = findDup xs (acc `Set.insert` freqSet) (acc+x)

main :: IO ()
main = do
    args    <- getArgs
    content <- readFile (args !! 0)
    let freqList = getFreq content
    print $ sum freqList
    print $ findDup (cycle freqList) Set.empty 0
