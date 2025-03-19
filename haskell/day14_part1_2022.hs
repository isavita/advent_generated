
import Data.List (foldl', minimumBy, maximumBy)
import Data.Map (Map, fromList, member, insert)
import qualified Data.Map as M
import Data.Function (on)

data Point = Point { x :: Int, y :: Int } deriving (Eq, Ord, Show)

add :: Point -> Point -> Point
add (Point x1 y1) (Point x2 y2) = Point (x1 + x2) (y1 + y2)

bounds :: [Point] -> (Point, Point)
bounds ps = (Point minX minY, Point (maxX + 1) (maxY + 1))
  where
    minX = minimum $ map x ps
    minY = minimum $ map y ps
    maxX = maximum $ map x ps
    maxY = maximum $ map y ps

fill :: Map Point Bool -> Int
fill grid = go 0 0 (fromList [(p, True) | p <- M.keys grid]) (Point 500 0)
    where
        floorY = y (snd (bounds (M.keys grid)))
        
        go firstFloorTouch sands grid' sand
            | y sand == floorY - 1 =
                let firstFloorTouch' = if firstFloorTouch == 0 then sands else firstFloorTouch
                    grid'' = insert sand True grid'
                in go firstFloorTouch' (sands + 1) grid'' (Point 500 0)
            | otherwise =
                let nexts = map (add sand) [Point 0 1, Point (-1) 1, Point 1 1]
                    next =  case filter (\p -> not $ member p grid') nexts of
                                [] -> Nothing
                                (n:_) -> Just n
                in case next of 
                    Nothing -> 
                        let grid'' = insert sand True grid'
                        in if y sand == 0 then firstFloorTouch
                           else go firstFloorTouch (sands+1) grid'' (Point 500 0)
                    Just newSand -> go firstFloorTouch sands grid' newSand
            

parsePoint :: String -> Point
parsePoint s = Point (read a) (read b)
  where
    [a, b] = wordsWhen (==',') s

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

parseLine :: String -> [Point]
parseLine = map parsePoint . wordsWhen (=='-') . map (\c -> if c == '>' then ' ' else c)

drawLine :: Map Point Bool -> Point -> Point -> Map Point Bool
drawLine grid p1 p2
    | x p1 == x p2 = foldl' (\acc y' -> insert (Point (x p1) y') True acc) grid ys
    | otherwise     = foldl' (\acc x' -> insert (Point x' (y p1)) True acc) grid xs
    where
        xs = if x p1 < x p2 then [x p1..x p2] else [x p2..x p1]
        ys = if y p1 < y p2 then [y p1..y p2] else [y p2..y p1]
    
createGrid :: [[Point]] -> Map Point Bool
createGrid pointLists = foldl' drawLine' M.empty pointLists
    where
        drawLine' grid points = foldl' (\acc (p1,p2) -> drawLine acc p1 p2 ) grid (zip points (tail points))

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let grid = createGrid $ map parseLine $ lines contents
    print $ fill grid
