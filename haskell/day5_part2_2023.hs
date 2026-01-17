
import Data.List (sort)

main :: IO ()
main = do
    content <- readFile "input.txt"
    let (seedsPart : mapsParts) = splitBlocks (lines content)
        seeds = parseSeeds seedsPart
        maps = map parseMap mapsParts
        finalRanges = foldl (\rs m -> concatMap (`applyRules` m) rs) seeds maps
    print $ minimum $ map fst finalRanges

splitBlocks :: [String] -> [[String]]
splitBlocks [] = []
splitBlocks ss = let (b, rest) = break (== "") ss in b : splitBlocks (dropWhile (== "") rest)

parseSeeds :: [String] -> [(Integer, Integer)]
parseSeeds (s:_) = pairs $ map read $ tail $ words s
  where pairs (x:y:zs) = (x, x + y) : pairs zs
        pairs _ = []

parseMap :: [String] -> [(Integer, Integer, Integer)]
parseMap (_:ls) = sort [(src, src + len, dst - src) | l <- ls, let [dst, src, len] = map read (words l)]

applyRules :: (Integer, Integer) -> [(Integer, Integer, Integer)] -> [(Integer, Integer)]
applyRules (s, e) [] = [(s, e)]
applyRules (s, e) ((srcS, srcE, off):rs)
    | s >= e    = []
    | e <= srcS = [(s, e)]
    | s >= srcE = applyRules (s, e) rs
    | otherwise = (if s < srcS then [(s, srcS)] else [])
                ++ [(max s srcS + off, min e srcE + off)]
                ++ (if e > srcE then applyRules (srcE, e) rs else [])

