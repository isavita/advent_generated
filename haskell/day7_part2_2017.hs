import Data.List
import Data.Maybe
import qualified Data.Map as Map

data Program = Program { name :: String, weight :: Int, children :: [String] } deriving (Show)

main = do
    contents <- readFile "input.txt"
    let programs = map parseLine (lines contents)
        programMap = Map.fromList [(name p, p) | p <- programs]
        root = findRoot programs
    putStrLn $ "Root: " ++ root
    _ <- findUnbalanced programMap root
    return ()

parseLine :: String -> Program
parseLine line = 
    let parts = words line
        name = head parts
        weight = read $ init $ tail $ parts !! 1
        children = if length parts > 2 then map (filter (/= ',')) (drop 3 parts) else []
    in Program name weight children

findRoot :: [Program] -> String
findRoot programs = 
    let allNames = map name programs
        childNames = concatMap children programs
        root = head $ allNames \\ childNames
    in root

findUnbalanced :: Map.Map String Program -> String -> IO Int
findUnbalanced programMap root = do
    let program = fromJust $ Map.lookup root programMap
    childWeights <- mapM (findUnbalanced programMap) (children program)
    if length (nub childWeights) > 1 
       then do
           let (unbalanced, correct) = findMismatch childWeights
               unbalancedChild = children program !! fromJust (elemIndex unbalanced childWeights)
               unbalancedProgram = fromJust $ Map.lookup unbalancedChild programMap
               diff = correct - unbalanced
           putStrLn $ "Unbalanced: " ++ name unbalancedProgram ++ ", Correct weight: " ++ show (weight unbalancedProgram + diff)
           return $ weight program + sum childWeights
       else return $ weight program + sum childWeights

findMismatch :: [Int] -> (Int, Int)
findMismatch weights = 
    let grouped = map (\x -> (head x, length x)) . group . sort $ weights
        unbalanced = fst $ head $ filter (\(_, count) -> count == 1) grouped
        correct = fst $ head $ filter (\(_, count) -> count > 1) grouped
    in (unbalanced, correct)