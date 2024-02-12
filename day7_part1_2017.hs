
import Data.List

parseInput :: String -> (String, [String])
parseInput line = case words line of
    (name:weight:rest) -> (name, map (filter (/=',')) rest)

getProgramsAbove :: [(String, [String])] -> [String]
getProgramsAbove programs = nub $ concatMap snd programs

findBottomProgram :: [(String, [String])] -> [(String, [String])] -> String
findBottomProgram [] allPrograms = head $ map fst $ filter (\(name, _) -> notElem name (getProgramsAbove allPrograms)) allPrograms
findBottomProgram ((name, _):ps) allPrograms = if name `elem` getProgramsAbove allPrograms
                                                then findBottomProgram ps allPrograms
                                                else name

main = do
    contents <- readFile "input.txt"
    let input = map parseInput $ lines contents
    let result = findBottomProgram input input
    putStrLn result
