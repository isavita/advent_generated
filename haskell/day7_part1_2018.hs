import Data.List (sort, nub)
import System.IO (readFile)

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let dependencies = map parseLine (lines contents)
        order = findOrder dependencies
    putStrLn order

parseLine :: String -> (Char, Char)
parseLine line = (line !! 5, line !! 36)

findOrder :: [(Char, Char)] -> String
findOrder dependencies = go [] (nub $ sort $ map fst dependencies ++ map snd dependencies)
  where
    go order [] = order
    go order available = let next = head $ sort $ filter (\step -> all (\(dep, _) -> dep `elem` order) (filter ((== step) . snd) dependencies)) available
                         in go (order ++ [next]) (filter (/= next) available)