import           System.Environment
import           Data.List
import qualified Data.Set as Set

type Point = (Int,Int)
type Rect  = (Int,Int,Int,Int)

--- Part One ---
toRect :: String -> Rect
toRect rect = (read x, read y, read w, read h)
    where (x:y:w:h:[]) = words rect

getRectPoints :: Rect -> [Point]
getRectPoints (x,y,w,h) = [(x',y') | x' <- [x..x+w-1], y' <- [y..y+h-1]]

getOverlapPoints :: (Rect,Rect) -> Set.Set Point
getOverlapPoints (r1,r2) = Set.fromList ps1 `Set.intersection` Set.fromList ps2
    where ps1 = getRectPoints r1
          ps2 = getRectPoints r2

getOverlapArea :: [Rect] -> Int
getOverlapArea rects   = Set.size $ foldl appendOverlaps Set.empty combinations
    where combinations = [(x,y) | (x:rest) <- tails rects, y <- rest]
          appendOverlaps acc rects = Set.union acc $ getOverlapPoints rects

main :: IO ()
main = do
    args    <- getArgs
    content <- readFile (args !! 0)
    let rects = map toRect $ lines content
    print $ getOverlapArea $ rects
