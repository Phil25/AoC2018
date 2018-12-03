import           System.Environment
import           Data.List
import qualified Data.Set as Set

{-
 - FAIR WARNING:
 - This code is fucking awful, I'm ashamed that it works
 - YOU HAVE BEEN WARNED
-}

type Point = (Int,Int)
type Rect  = (Int,Int,Int,Int,Int) -- id, x, y, width, height

combinations :: [a] -> [(a,a)]
combinations xs = [(x,y) | (x:rest) <- tails xs, y <- rest]

--- Part One ---
toRect :: String -> Rect
toRect rect = (read i, read x, read y, read w, read h)
    where (i:x:y:w:h:[]) = words rect

getRectPoints :: Rect -> [Point]
getRectPoints (_,x,y,w,h) = [(x',y') | x' <- [x..x+w-1], y' <- [y..y+h-1]]

getOverlapPoints :: (Rect,Rect) -> Set.Set Point
getOverlapPoints (r1,r2) = Set.fromList ps1 `Set.intersection` Set.fromList ps2
    where ps1 = getRectPoints r1
          ps2 = getRectPoints r2

getOverlapArea :: [Rect] -> Int
getOverlapArea rects = Set.size $ foldl appendOverlaps Set.empty (combinations rects)
    where appendOverlaps acc rects = Set.union acc $ getOverlapPoints rects

--- Part Two ---
getId :: Rect -> Int
getId (i,_,_,_,_) = i

overlaps :: Rect -> [Rect] -> Bool
overlaps _ [] = False
overlaps r1 (r2:rest)
    | getId r1 == getId r2                       = overlaps r1 rest
    | (Set.size $ getOverlapPoints (r1,r2)) /= 0 = True
    | otherwise                                  = overlaps r1 rest

getNoOverlap :: [Rect] -> [Rect] -> Rect
getNoOverlap [] _ = (0,0,0,0,0)
getNoOverlap (r:rs) all
    | overlaps r all = getNoOverlap rs all
    | otherwise      = r

main :: IO ()
main = do
    args    <- getArgs
    content <- readFile (args !! 0)
    let rects = map toRect $ lines content
    print $ getOverlapArea $ rects
    print $ getId $ getNoOverlap rects rects
