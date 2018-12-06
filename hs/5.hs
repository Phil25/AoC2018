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

main :: IO ()
main = do
    args    <- getArgs
    content <- readFile (args !! 0)
    print $ length (react content) -1
