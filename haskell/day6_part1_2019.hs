import qualified Data.Map as Map
import System.IO

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let orbits = map (split ')') (lines contents)
        orbitMap = foldl (\acc (a, b) -> Map.insert b a acc) Map.empty orbits
        totalOrbits = sum $ map (countOrbits orbitMap) (Map.keys orbitMap)
    print totalOrbits

split :: Eq a => a -> [a] -> ([a], [a])
split _ [] = ([], [])
split delim str = let (before, after) = break (== delim) str
                  in (before, tail after)

countOrbits :: Map.Map String String -> String -> Int
countOrbits orbitMap obj = case Map.lookup obj orbitMap of
    Nothing -> 0
    Just parent -> 1 + countOrbits orbitMap parent