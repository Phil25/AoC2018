import System.Environment

--- Part One ---
readFreq :: String -> Int
readFreq ('-':xs) = negate $ read xs
readFreq (_:xs) = read xs

getFreq :: String -> [Int]
getFreq = map readFreq . lines

--- Part Two ---
findDup :: [Int] -> [Int] -> Int -> Int
findDup (x:xs) freqHistory acc
    | acc `elem` freqHistory = acc
    | otherwise              = findDup xs (acc:freqHistory) (acc+x)

main :: IO ()
main = do
    args    <- getArgs
    content <- readFile (args !! 0)
    let freqList = getFreq content
    print $ sum freqList
    print $ findDup (cycle freqList) [] 0
