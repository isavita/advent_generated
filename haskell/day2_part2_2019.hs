import System.IO
import Data.List
import Data.Maybe

main = do
    contents <- readFile "input.txt"
    let program = map read $ splitOn "," contents
    let result = findNounVerb program 19690720
    case result of
        Just (noun, verb) -> print $ 100 * noun + verb
        Nothing -> putStrLn "No solution found"

splitOn :: Eq a => [a] -> [a] -> [[a]]
splitOn _ [] = []
splitOn delim str = case breakList delim str of
    (before, after) -> before : splitOn delim (drop (length delim) after)

breakList :: Eq a => [a] -> [a] -> ([a], [a])
breakList _ [] = ([], [])
breakList delim str@(x:xs)
    | isPrefixOf delim str = ([], drop (length delim - 1) str)
    | otherwise = let (before, after) = breakList delim xs in (x:before, after)

findNounVerb :: [Int] -> Int -> Maybe (Int, Int)
findNounVerb program target = listToMaybe [(noun, verb) | noun <- [0..99], verb <- [0..99], runProgram (replaceInputs program noun verb) == target]

replaceInputs :: [Int] -> Int -> Int -> [Int]
replaceInputs program noun verb = take 1 program ++ [noun, verb] ++ drop 3 program

runProgram :: [Int] -> Int
runProgram program = head $ execProgram 0 program

execProgram :: Int -> [Int] -> [Int]
execProgram pos program
    | opcode == 99 = program
    | opcode == 1 = execProgram (pos + 4) (performOperation (+))
    | opcode == 2 = execProgram (pos + 4) (performOperation (*))
    | otherwise = error "Unknown opcode"
    where
        opcode = program !! pos
        performOperation op = let a = program !! (program !! (pos + 1))
                                  b = program !! (program !! (pos + 2))
                                  dest = program !! (pos + 3)
                              in take dest program ++ [op a b] ++ drop (dest + 1) program