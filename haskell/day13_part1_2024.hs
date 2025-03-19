
import Data.Char (isDigit)
import Data.List (isPrefixOf, stripPrefix)

parseVal :: String -> Int
parseVal = read . dropWhile (not . isDigit)

parseLine :: String -> (Int, Int)
parseLine s = (parseVal x, parseVal y)
  where
    [x, y] = map (dropWhile (== ' ') . drop 2) $ wordsWhen (== ',') s

parsePrize :: String -> (Int, Int)
parsePrize = parseLine

parseMachine :: [String] -> [(String, (Int, Int))]
parseMachine lines = map parseLine' $ filter (not . null) lines
    where 
        parseLine' l
            | "Button A:" `isPrefixOf` l = ("A", parseLine $ drop 9 l)
            | "Button B:" `isPrefixOf` l = ("B", parseLine $ drop 9 l)
            | "Prize:"    `isPrefixOf` l = ("P", parsePrize $ drop 6 l)
            

solveMachine :: [(String, (Int, Int))] -> Int
solveMachine m = minimumOrMinus1 [a * 3 + b | a <- [0..101], b <- [0..101],
                                    ax * a + bx * b == px,
                                    ay * a + by * b == py]
  where
    val s = case lookup s m of Just (x,_) -> x; Nothing -> error "Missing Value"
    val2 s = case lookup s m of Just (_,y) -> y; Nothing -> error "Missing Value"
    ax = val "A"
    ay = val2 "A"
    bx = val "B"
    by = val2 "B"
    px = val "P"
    py = val2 "P"
    
minimumOrMinus1 :: [Int] -> Int
minimumOrMinus1 [] = -1
minimumOrMinus1 xs = minimum xs

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
  "" -> []
  s' -> w : wordsWhen p s''
    where
      (w, s'') = break p s'

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let machines = map parseMachine . splitWhen null . lines $ contents
  let results = filter (/= -1) $ map solveMachine machines
  let (count, total) = (length results, sum results)
  putStrLn $ if null results then "0 0" else show count ++ " " ++ show total

splitWhen :: (a -> Bool) -> [a] -> [[a]]
splitWhen p xs = case break p xs of
  (ys, []) -> [ys]
  (ys, zs) -> ys : splitWhen p (drop 1 zs)
