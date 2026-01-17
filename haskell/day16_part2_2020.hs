
import Data.List
import System.IO

main :: IO ()
main = do
    content <- readFile "input.txt"
    let blocks = splitBlocks (lines content)
        rules = map parseRule (blocks !! 0)
        myTicket = map read $ splitBy ',' (blocks !! 1 !! 1)
        nearby = map (map read . splitBy ',') (drop 1 (blocks !! 2))
        validNearby = filter (all (\v -> any (\(_, f) -> f v) rules)) nearby
        n = length myTicket
        cols = transpose validNearby
        possible = [(name, [i | (i, col) <- zip [0..] cols, all f col]) | (name, f) <- rules]
        mapping = solve possible
        ans = product [toInteger (myTicket !! i) | (name, i) <- mapping, "departure" `isPrefixOf` name]
    print ans

splitBlocks :: [String] -> [[String]]
splitBlocks [] = []
splitBlocks xs = case break (== "") xs of
    (a, b) -> a : if null b then [] else splitBlocks (tail b)

splitBy :: Eq a => a -> [a] -> [[a]]
splitBy c s = case break (== c) s of
    (a, b) -> a : if null b then [] else splitBy c (tail b)

parseRule :: String -> (String, Int -> Bool)
parseRule s =
    let (name, rest) = break (== ':') s
        ps = words (drop 2 rest)
        parseR r = let (a, b) = break (== '-') r in (read a, read (tail b))
        (min1, max1) = parseR (ps !! 0)
        (min2, max2) = parseR (ps !! 2)
    in (name, \v -> (v >= min1 && v <= max1) || (v >= min2 && v <= max2))

solve :: [(String, [Int])] -> [(String, Int)]
solve ps
    | all ((== 1) . length . snd) ps = map (\(n, [p]) -> (n, p)) ps
    | otherwise =
        let fixed = [p | (_, [p]) <- ps]
            next = map (\(n, p) -> if length p > 1 then (n, p \\ fixed) else (n, p)) ps
        in solve next

