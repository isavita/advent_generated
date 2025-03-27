
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.List (foldl')

type Point = (Int, Int)
type Image = M.Map Point Bool
type Bounds = (Point, Point) -- ((minR, minC), (maxR, maxC))

boolToInt :: Bool -> Int
boolToInt True  = 1
boolToInt False = 0

parseInput :: String -> (String, Image, Bounds)
parseInput content =
    let (algoLine : _ : imageLines) = lines content
        rows = length imageLines
        cols = if rows > 0 then length (head imageLines) else 0
        points = [ ((r, c), char == '#')
                 | (r, line) <- zip [0..] imageLines
                 , (c, char) <- zip [0..] line
                 ]
        initialMap = M.fromList points
        initialBounds = ((0, 0), (rows - 1, cols - 1))
    in (algoLine, initialMap, initialBounds)

getNeighborIndex :: Image -> Bool -> Point -> Int
getNeighborIndex image background (r, c) =
    let neighbors = [ (r + dr, c + dc) | dr <- [-1..1], dc <- [-1..1] ]
        bits = map (\p -> fromMaybe background (M.lookup p image)) neighbors
    in foldl' (\acc bit -> acc * 2 + boolToInt bit) 0 bits

step :: String -> (Image, Bounds, Bool) -> (Image, Bounds, Bool)
step algo (!image, ((minR, minC), (maxR, maxC)), !background) =
    let !minR' = minR - 1
        !minC' = minC - 1
        !maxR' = maxR + 1
        !maxC' = maxC + 1
        !newBounds = ((minR', minC'), (maxR', maxC'))

        coordsToUpdate = [ (r, c) | r <- [minR'..maxR'], c <- [minC'..maxC'] ]

        newImage = M.fromList [ (p, (algo !! getNeighborIndex image background p) == '#')
                              | p <- coordsToUpdate ]

        !nextBackgroundIndex = if background then 511 else 0
        !nextBackground = (algo !! nextBackgroundIndex) == '#'

    in (newImage, newBounds, nextBackground)

main :: IO ()
main = do
    content <- readFile "input.txt"
    let (algorithm, initialImage, initialBounds) = parseInput content
        iterations = 50

    let (finalImage, _, _) = foldl' (\st _ -> step algorithm st)
                                  (initialImage, initialBounds, False)
                                  [1..iterations]

    let litPixelCount = M.size $ M.filter id finalImage
    print litPixelCount
