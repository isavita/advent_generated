
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List (lines)
import Data.Maybe (mapMaybe)
import GHC.Real (gcd) -- Provides non-negative gcd

-- Generates combinations of size k
combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations k (x:xs) = map (x:) (combinations (k-1) xs) ++ combinations k xs

-- Calculates normalized line parameters (sx, sy, c) for sy * x - sx * y = c
-- Returns Nothing if points are identical
calculateLine :: (Int, Int) -> (Int, Int) -> Maybe (Int, Int, Int)
calculateLine a@(y1, x1) b@(y2, x2) =
    let dy = y2 - y1
        dx = x2 - x1
    in if dx == 0 && dy == 0 then Nothing
       else Just $ normalizeLine dy dx a

-- Normalizes line direction (dx, dy) and calculates constant c
normalizeLine :: Int -> Int -> (Int, Int) -> (Int, Int, Int)
normalizeLine dy dx (y1, x1) =
    let g = gcd dy dx -- g >= 0
        sy' = dy `div` g
        sx' = dx `div` g
        -- Normalize direction: sx >= 0. If sx == 0, then sy >= 0.
        (sx, sy) = if sx' < 0 || (sx' == 0 && sy' < 0)
                     then (-sx', -sy')
                     else (sx', sy')
        -- Calculate c for sy * x - sx * y = c
        c = sy * x1 - sx * y1
    in (sx, sy, c)

-- Finds all grid points within (h, w) bounds lying on the line (sx, sy, c)
pointsOnLine :: Int -> Int -> (Int, Int, Int) -> Set.Set (Int, Int)
pointsOnLine h w (sx, sy, c)
    | sx == 0 && sy == 0 = Set.empty -- Degenerate case
    | sy == 0 = -- Horizontal line: -sx * y = c => y = -c / sx
        if sx /= 0 && c `mod` sx == 0 then -- sx > 0 guaranteed by normalization
            let y = negate c `div` sx
            in if 0 <= y && y < h
               then Set.fromList [(y, x) | x <- [0 .. w - 1]]
               else Set.empty
        else Set.empty
    | sx == 0 = -- Vertical line: sy * x = c => x = c / sy
        if sy /= 0 && c `mod` sy == 0 then -- sy > 0 guaranteed by normalization
            let x = c `div` sy
            in if 0 <= x && x < w
               then Set.fromList [(y, x) | y <- [0 .. h - 1]]
               else Set.empty
        else Set.empty
    | otherwise = -- General line: sy * x - sx * y = c => sy * x = c + sx * y
                  -- sx > 0 guaranteed by normalization
        Set.fromList [ (y, x)
                     | y <- [0 .. h - 1]
                     , let val = c + sx * y
                     , val `mod` sy == 0 -- Check if val is divisible by sy
                     , let x = val `div` sy -- Use standard integer division
                     , 0 <= x && x < w   -- Check bounds for x
                     ]

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let grid = lines contents
        h = length grid
        w = if h == 0 then 0 else length (head grid)

        -- Create Map: Char -> [(y, x)] associating frequency with antenna coordinates
        indexedGrid = zip [0..] (map (zip [0..]) grid)
        antennaCoords = [ (c, [(y, x)]) -- Use singleton list for Map.fromListWith
                        | (y, row) <- indexedGrid
                        , (x, c)   <- row
                        , c /= '.'
                        ]
        antennas = Map.fromListWith (++) antennaCoords

        -- Create Map: Char -> Set (sx, sy, c) of unique lines per frequency
        linesPerFreq = Map.map processAntennaList antennas
          where
            processAntennaList coords = Set.fromList $ mapMaybe getLineFromPair (combinations 2 coords)
            getLineFromPair [a, b] = calculateLine a b
            getLineFromPair _      = Nothing -- Should not be reached by combinations 2

        -- Combine all unique lines from all frequencies into a single set
        allLines = Set.unions (Map.elems linesPerFreq)

        -- Calculate all unique grid points ('antinodes') lying on any of these lines
        antinodes = Set.unions $ Set.map (pointsOnLine h w) allLines

    -- Print the total number of unique antinodes
    print $ Set.size antinodes
