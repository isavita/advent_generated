
import System.IO

main = do
    contents <- readFile "input.txt"
    let numbers = map read $ words contents :: [Int]
    let (sum, _) = parseTree numbers 0
    print sum

parseTree :: [Int] -> Int -> (Int, Int)
parseTree inputData index = 
    let childCount = inputData !! index
        metaCount = inputData !! (index + 1)
        (sum, newIndex) = parseChildren inputData (index + 2) childCount
        totalSum = sum + sumMetadata inputData newIndex metaCount
    in (totalSum, newIndex + metaCount)

parseChildren :: [Int] -> Int -> Int -> (Int, Int)
parseChildren inputData index 0 = (0, index)
parseChildren inputData index childCount = 
    let (childSum, newIndex) = parseTree inputData index
        (nextSum, nextIndex) = parseChildren inputData newIndex (childCount - 1)
    in (childSum + nextSum, nextIndex)

sumMetadata :: [Int] -> Int -> Int -> Int
sumMetadata inputData index 0 = 0
sumMetadata inputData index metaCount = 
    inputData !! index + sumMetadata inputData (index + 1) (metaCount - 1)
