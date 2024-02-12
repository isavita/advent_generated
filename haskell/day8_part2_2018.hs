
import System.IO

main = do
    contents <- readFile "input.txt"
    let numbers = map read $ words contents :: [Int]
    let (value, _) = parseTree numbers 0
    print value

parseTree :: [Int] -> Int -> (Int, Int)
parseTree inputData index = 
    let childCount = inputData !! index
        metaCount = inputData !! (index + 1)
        newIndex = index + 2
        (childValues, finalIndex) = parseChildren inputData newIndex childCount
        value = if childCount == 0
                then sum $ take metaCount $ drop finalIndex inputData
                else sum $ map (\x -> childValues !! (x-1)) $ filter (\x -> x > 0 && x <= childCount) $ take metaCount $ drop finalIndex inputData
    in (value, finalIndex + metaCount)

parseChildren :: [Int] -> Int -> Int -> ([Int], Int)
parseChildren inputData index count
    | count == 0 = ([], index)
    | otherwise = let (childValue, nextIndex) = parseTree inputData index
                      (restChildren, finalIndex) = parseChildren inputData nextIndex (count-1)
                  in (childValue : restChildren, finalIndex)
