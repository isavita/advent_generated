import qualified Data.Set as Set
import System.IO

type Point = (Int, Int, Int)

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let points = map parsePoint (lines contents)
        pointSet = Set.fromList points
        totalSurfaceArea = sum [surfaceArea pointSet point | point <- points]
    print totalSurfaceArea

parsePoint :: String -> Point
parsePoint str = let [x, y, z] = map read (wordsWhen (== ',') str) in (x, y, z)

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s''
        where (w, s'') = break p s'

surfaceArea :: Set.Set Point -> Point -> Int
surfaceArea points (x, y, z) = 6 - connectedSides
    where connectedSides = length $ filter (\p -> Set.member p points) neighbors
          neighbors = [(x+1, y, z), (x-1, y, z), (x, y+1, z), (x, y-1, z), (x, y, z+1), (x, y, z-1)]