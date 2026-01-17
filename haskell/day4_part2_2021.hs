
import Data.List (transpose, partition)

type Board = [[Int]]

main :: IO ()
main = do
    content <- readFile "input.txt"
    let ls = lines content
        nums = map read $ splitBy (==',') (head ls)
        boards = toBoards . map read . words . unlines $ tail ls
    print $ solve nums [] boards 0

splitBy :: (Char -> Bool) -> String -> [String]
splitBy p s = case dropWhile p s of
    "" -> []
    s' -> let (w, s'') = break p s' in w : splitBy p s''

toBoards :: [Int] -> [Board]
toBoards [] = []
toBoards xs = let (this, rest) = splitAt 25 xs
              in chunksOf 5 this : toBoards rest

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = let (a, b) = splitAt n xs in a : chunksOf n b

isWin :: [Int] -> Board -> Bool
isWin drawn b = any (all (`elem` drawn)) (b ++ transpose b)

score :: [Int] -> Board -> Int
score drawn b = sum [x | r <- b, x <- r, x `notElem` drawn]

solve :: [Int] -> [Int] -> [Board] -> Int -> Int
solve (n:ns) drawn boards lastScore
    | null boards = lastScore
    | otherwise =
        let nextDrawn = n : drawn
            (winners, losers) = partition (isWin nextDrawn) boards
            nextScore = if null winners then lastScore else score nextDrawn (last winners) * n
        in solve ns nextDrawn losers nextScore
solve _ _ _ lastScore = lastScore

