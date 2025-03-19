
import qualified Data.Map as M
import Data.Char (isDigit)
import Control.Monad (forM_)

type Point = (Int, Int)
type Grid = M.Map Point Char
data Part = Part { xmin :: Int, xmax :: Int, y :: Int, n :: Int } deriving (Show)

neighbors8 :: [Point]
neighbors8 = [(0, 1), (0, -1), (1, 0), (-1, 0), (-1, -1), (-1, 1), (1, -1), (1, 1)]

parse :: String -> (Grid, [Part])
parse input = (grid, parts)
    where
        lines_ = lines input
        grid = M.fromList [((x, y), c) | (y, line) <- zip [0..] lines_, (x, c) <- zip [0..] line]
        parts = parseParts 0 lines_ [] Nothing

        parseParts _ [] acc _ = reverse acc
        parseParts y (line:rest) acc curr =
            let (newAcc, newCurr) = processLine y 0 line acc curr
            in parseParts (y+1) rest newAcc newCurr

        processLine _ _ [] acc Nothing = (acc, Nothing)
        processLine y x [] acc (Just p) = (p:acc, Nothing)
        processLine y x (c:cs) acc curr
            | isDigit c =
                let digit = fromEnum c - fromEnum '0'
                in case curr of
                    Nothing -> processLine y (x+1) cs acc (Just (Part x x y digit))
                    Just p  -> processLine y (x+1) cs acc (Just (Part (xmin p) x y (n p * 10 + digit)))
            | otherwise =
                case curr of
                    Nothing -> processLine y (x+1) cs acc Nothing
                    Just p  -> processLine y (x+1) cs (p:acc) Nothing

solve :: (Grid, [Part]) -> Int
solve (grid, parts) = sumVal
  where
    partsGrid = M.fromList [((x, p_y), i) | (i, Part xmin xmax p_y _) <- zip [0..] parts, x <- [xmin..xmax]]

    sumVal = sum [product | (p, c) <- M.toList grid, c == '*', let neighborParts = neighborIndices p, length neighborParts == 2, let product = productOfParts neighborParts]
      
    neighborIndices :: Point -> [Int]
    neighborIndices (px, py) =
      foldr (\(dx,dy) acc ->
        case M.lookup (px+dx, py+dy) partsGrid of
          Just index -> if index `elem` acc then acc else index:acc
          Nothing    -> acc
      ) [] neighbors8

    productOfParts :: [Int] -> Int
    productOfParts indices = product [n (parts !! i) | i <- indices]

main :: IO ()
main = do
    input <- readFile "input.txt"
    let result = solve . parse $ input
    print result
