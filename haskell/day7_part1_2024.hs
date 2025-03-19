
import Data.List (intercalate)
import Control.Monad (forM)

parseInput :: String -> [(Int, [Int])]
parseInput = map parseLine . lines
  where
    parseLine line =
      let (testValueStr, numbersStr) = break (== ':') line
          testValue = read testValueStr :: Int
          numbers = map read (words (drop 2 numbersStr)) :: [Int]
      in (testValue, numbers)

evaluateExpression :: Int -> [Int] -> Maybe String
evaluateExpression testValue numbers =
  let operators = ['+', '*']
      numOperators = length numbers - 1
      allOps = sequence (replicate numOperators operators)
  in foldr (\ops acc -> case acc of
    Just _ -> acc
    Nothing -> checkExpression ops) Nothing allOps

  where
    checkExpression ops =
        let result = foldl (\acc (op, num) -> if op == '+' then acc + num else acc * num) (head numbers) (zip ops (tail numbers))
            expression = show (head numbers) ++ concatMap (\(op, num) -> " " ++ [op] ++ " " ++ show num) (zip ops (tail numbers))
        in if result == testValue then Just expression else Nothing

calculateSumOfTestValues :: [(Int, [Int])] -> Int
calculateSumOfTestValues = sum . map fst . filter (\(testValue, numbers) -> evaluateExpression testValue numbers /= Nothing)

main :: IO ()
main = do
  input <- readFile "input.txt"
  let testValues = parseInput (init input)
  print (calculateSumOfTestValues testValues)
