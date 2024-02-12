
import Data.List

countCombinations :: Int -> [Int] -> Int
countCombinations target containers = length $ filter (\comb -> sum comb == target) $ subsequences containers

main = do
    input <- readFile "input.txt"
    let containers = map read $ lines input
    let totalLiters = 150
    let allCombinations = countCombinations totalLiters containers
    print allCombinations

    let minContainers = minimum $ map length $ filter (\comb -> sum comb == totalLiters) $ subsequences containers
    let waysWithMinContainers = length $ filter (\comb -> sum comb == totalLiters && length comb == minContainers) $ subsequences containers
    print waysWithMinContainers
