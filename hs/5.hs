import System.Environment
import Data.Char

--- Part One ---
reactable :: Char -> Char -> Bool
reactable a b
    | ua && not ub = a == toUpper b
    | not ua && ub = b == toUpper a
    | otherwise    = False
        where ua   = isUpper a
              ub   = isUpper b

react :: String -> String
react = foldr step ""
    where
        step x (a:as)
            | reactable x a = as
        step x as           = x : as

--- Part Two ---
withRemoved :: String -> Char -> Int
withRemoved xs c = length (react xsNoC) -1
    where xsNoC  = [x | x <- xs, not $ x `elem` [c, toUpper c]]

getSmallest :: Int -> String -> Int
getSmallest init xs = foldl step init ['a'..'z']
    where step n c = min n $ xs `withRemoved` c

main :: IO ()
main = do
    args    <- getArgs
    content <- readFile (args !! 0)
    let len = length (react content) -1
    print $ len
    print $ getSmallest len content
