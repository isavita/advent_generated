
import qualified Data.List as L
import qualified Data.Set as Set
import Data.Maybe (mapMaybe)
import System.IO (readFile)
import Data.Char (isUpper, isLower)

-- --- Data Types ---

-- A replacement rule maps a 'from' string to a 'to' string
type Rule = (String, String)

-- Input consists of a list of rules and the target molecule string
type Input = ([Rule], String)

-- --- Parsing ---

-- Parses a single line into a Rule, returns Nothing if format is wrong
parseRule :: String -> Maybe Rule
parseRule line = case L.words line of
    [from, "=>", to] -> Just (from, to)
    _                -> Nothing

-- Parses the entire input file content
parseInput :: String -> Input
parseInput content =
    let ls = lines content
        -- Split lines into rules and the molecule based on the blank line
        (ruleLines, rest) = L.break null ls
        -- Parse rules, filtering out any malformed lines
        rules = mapMaybe parseRule ruleLines
        -- The molecule is the first non-empty line after the blank line
        molecule = case dropWhile null rest of
                     (m:_) -> m
                     []    -> error "No molecule found in input"
    in (rules, molecule)

-- --- Part 1 ---

-- Finds all starting indices of a substring (needle) in a string (haystack)
findIndices :: String -> String -> [Int]
findIndices needle haystack =
    L.findIndices (L.isPrefixOf needle) (L.tails haystack)

-- Replaces a substring of a given length at a specific index
replaceAt :: Int -> Int -> String -> String -> String
replaceAt idx len replacement original =
    take idx original ++ replacement ++ drop (idx + len) original

-- Generates all possible molecules by applying one rule once
generateMolecules :: [Rule] -> String -> [String]
generateMolecules rules molecule = concatMap applyRule rules
  where
    applyRule :: Rule -> [String]
    applyRule (from, to) =
        let indices = findIndices from molecule
            lenFrom = length from
        in map (\idx -> replaceAt idx lenFrom to molecule) indices

-- Solves Part 1: Counts the number of distinct molecules generated
solvePart1 :: Input -> Int
solvePart1 (rules, molecule) =
    Set.size $ Set.fromList $ generateMolecules rules molecule

-- --- Part 2 ---

-- Tokenizes the molecule string into a list of elements
-- e.g., "HOH" -> ["H", "O", "H"], "CRnCaSi" -> ["C", "Rn", "Ca", "Si"]
tokenize :: String -> [String]
tokenize [] = []
tokenize (c:cs)
    | isUpper c = case span isLower cs of
                    (lowercaseSuffix, rest) -> (c : lowercaseSuffix) : tokenize rest
    -- Assuming input molecule only contains valid element symbols
    | otherwise = error $ "Unexpected character in molecule: " ++ [c]

-- Counts occurrences of a specific element in a tokenized list
countElem :: String -> [String] -> Int
countElem x = L.length . L.filter (== x)

-- Solves Part 2 using the derived formula based on puzzle input structure:
-- Steps = TotalElements - Count(Rn) - Count(Ar) - 2 * Count(Y) - 1
-- This formula relies on specific properties of the provided puzzle input's grammar.
solvePart2 :: String -> Int
solvePart2 molecule =
    let tokens = tokenize molecule
        totalElements = length tokens
        countRn = countElem "Rn" tokens
        countAr = countElem "Ar" tokens
        countY  = countElem "Y" tokens
    in totalElements - countRn - countAr - 2 * countY - 1

-- --- Main Execution ---

main :: IO ()
main = do
    -- Read input from the file
    content <- readFile "input.txt"

    -- Parse the input
    let inputData@(rules, molecule) = parseInput content

    -- Solve and print Part 1
    let part1Result = solvePart1 inputData
    putStrLn $ "Part 1: " ++ show part1Result

    -- Solve and print Part 2
    -- Note: Part 2 only needs the molecule string from the parsed input
    let part2Result = solvePart2 molecule
    putStrLn $ "Part 2: " ++ show part2Result
