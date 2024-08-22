import qualified Data.Map as M
import Data.List (sort, group)

main = do
    contents <- readFile "input.txt"
    let (template:_:rules) = lines contents
        rulesMap = M.fromList [(take 2 rule, last rule) | rule <- rules]
        finalPolymer = iteratePolymer template rulesMap 10
        counts = M.fromListWith (+) [(c, 1) | c <- finalPolymer]
        sortedCounts = sort [count | (_, count) <- M.toList counts]
        result = last sortedCounts - head sortedCounts
    print result

iteratePolymer :: String -> M.Map String Char -> Int -> String
iteratePolymer polymer rules steps
    | steps == 0 = polymer
    | otherwise = iteratePolymer newPolymer rules (steps - 1)
    where newPolymer = concatMap (\(a, b) -> [a, rules M.! [a, b]]) (zip polymer (tail polymer)) ++ [last polymer]