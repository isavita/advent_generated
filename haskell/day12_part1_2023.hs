
import Data.List (splitAt)
import Data.Char (isDigit)
import Data.Maybe (fromMaybe)
import qualified Data.Map as M

data Row = Row { springs :: String, group :: [Int] } deriving (Show, Eq)

parseInput :: String -> [Row]
parseInput = map parseLine . lines
  where
    parseLine :: String -> Row
    parseLine line = Row s (map read $ splitCommas nums)
      where
        (s, ' ':nums) = splitAt (head $ findIndices isSpace line) line
        findIndices p = foldr (\(i, x) acc -> if p x then i:acc else acc) [] . zip [0..]
        splitCommas :: String -> [String]
        splitCommas xs = case break (==',') xs of
            (ys, []) -> [ys]
            (ys, _:zs) -> ys : splitCommas zs

countArrangements :: Row -> Int
countArrangements row = countArrangementsRecursive row 0 0 0 M.empty

countArrangementsRecursive :: Row -> Int -> Int -> Int -> M.Map (Int, Int, Int) Int -> Int
countArrangementsRecursive row iSprings iGroup iContiguousDamaged cache =
    fromMaybe (calculateArrangements row iSprings iGroup iContiguousDamaged cache) (M.lookup key cache)
  where
    key = (iSprings, iGroup, iContiguousDamaged)

    calculateArrangements :: Row -> Int -> Int -> Int -> M.Map (Int, Int, Int) Int -> Int
    calculateArrangements row iSprings iGroup iContiguousDamaged cache =
        let
            result = case () of
                _ | iSprings == length (springs row) ->
                    if iGroup == length (group row) && iContiguousDamaged == 0
                    then 1
                    else if iGroup == length (group row) - 1 && iContiguousDamaged == (group row !! iGroup)
                    then 1
                    else 0
                _ ->
                    let
                        char = (springs row) !! iSprings
                        res1 = if char == '.' || char == '?'
                                then if iContiguousDamaged == 0
                                     then countArrangementsRecursive row (iSprings + 1) iGroup iContiguousDamaged cache
                                     else if iContiguousDamaged == (group row !! iGroup)
                                          then countArrangementsRecursive row (iSprings + 1) (iGroup + 1) 0 cache
                                          else 0
                                else 0
                        res2 = if char == '#' || char == '?'
                                then if iGroup < length (group row) && iContiguousDamaged < (group row !! iGroup)
                                     then countArrangementsRecursive row (iSprings + 1) iGroup (iContiguousDamaged + 1) cache
                                     else 0
                                else 0
                    in res1 + res2
        in
            M.insert key result cache `seq` result

unfoldRow :: Row -> Int -> Row
unfoldRow row unfoldingFactor = Row newSprings newGroup
  where
    newSprings = concat $ intersperse "?" $ replicate unfoldingFactor (springs row)
    newGroup = concat $ replicate unfoldingFactor (group row)
    intersperse :: a -> [a] -> [a]
    intersperse _ [] = []
    intersperse _ [x] = [x]
    intersperse sep (x:xs) = x : sep : intersperse sep xs

solve :: [Row] -> Int
solve rows = sum $ map countArrangements rows

main :: IO ()
main = do
    input <- readFile "input.txt"
    let rows = parseInput input
    print (solve rows)

isSpace :: Char -> Bool
isSpace c = c == ' '
