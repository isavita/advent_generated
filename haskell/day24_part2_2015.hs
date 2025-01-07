
import Data.List (sort, subsequences)
import Data.Maybe (fromMaybe)

solve :: Int -> [Int] -> Maybe Integer
solve groups weights = do
  let totalWeight = sum weights
  guard (totalWeight `rem` groups == 0)
  let targetWeight = totalWeight `div` groups
  let sortedWeights = sort weights
  let possibleFirstGroups =
        concatMap
          (filter ((== targetWeight) . sum) . subsequences)
          [sortedWeights]
  guard (not $ null possibleFirstGroups)
  let minGroupSize = minimum $ map length possibleFirstGroups
  let minSizeGroups = filter ((== minGroupSize) . length) possibleFirstGroups
  let qes = map (product . map fromIntegral) minSizeGroups
  return $ minimum qes

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let weights = map read $ lines contents :: [Int]

  let part1Result = solve 3 weights
  putStrLn $ "Part 1: " ++ show (fromMaybe (-1) part1Result)

  let part2Result = solve 4 weights
  putStrLn $ "Part 2: " ++ show (fromMaybe (-1) part2Result)

guard :: Bool -> Maybe ()
guard True = Just ()
guard False = Nothing
