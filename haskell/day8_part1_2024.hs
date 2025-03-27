
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List (tails)
import System.IO (readFile)

main :: IO ()
main = do
    contents <- readFile "input.txt"
    print $ solve contents

solve :: String -> Int
solve contents =
    let grid = lines contents
        h = length grid
        w = if h > 0 then length (head grid) else 0

        antennas = Map.fromListWith (++)
            [ (c, [(y, x)])
            | (y, row) <- zip [0..] grid
            , (x, c)  <- zip [0..] row
            , c /= '.'
            ]

        antinodes = Set.fromList
            [ p
            | coords <- Map.elems antennas
            , (a@(ay, ax), b@(by, bx)) <- pairs coords
            , p <- calculateAntinodes a b
            , isInBounds p
            ]
          where
            pairs :: [a] -> [(a, a)]
            pairs l = [(x, y) | (x:ys) <- tails l, y <- ys]

            calculateAntinodes (ay, ax) (by, bx) =
                [(2 * ay - by, 2 * ax - bx), (2 * by - ay, 2 * bx - ax)]

            isInBounds (y, x) = y >= 0 && y < h && x >= 0 && x < w

    in Set.size antinodes
