import qualified Data.Set as Set
import System.IO

main = do
    contents <- readFile "input.txt"
    let (dots, folds) = parseInput contents
        firstFold = head folds
        foldedDots = foldPaper dots firstFold
    print $ Set.size foldedDots

parseInput :: String -> (Set.Set (Int, Int), [(Char, Int)])
parseInput input = (dots, folds)
  where
    (dotLines, foldLines) = break null $ lines input
    dots = Set.fromList [(read x, read y) | line <- dotLines, let [x, y] = wordsWhen (== ',') line]
    folds = [(head axis, read value) | line <- tail foldLines, let [axis, value] = wordsWhen (== '=') $ drop 11 line]

foldPaper :: Set.Set (Int, Int) -> (Char, Int) -> Set.Set (Int, Int)
foldPaper dots (axis, foldLine) = Set.map foldDot dots
  where
    foldDot (x, y)
        | axis == 'x' && x > foldLine = (foldLine - (x - foldLine), y)
        | axis == 'y' && y > foldLine = (x, foldLine - (y - foldLine))
        | otherwise = (x, y)

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s''
        where (w, s'') = break p s'