
{-# LANGUAGE BangPatterns #-}

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import System.IO (readFile)

-- Type alias for position
type Pos = (Int, Int)

-- Data types to represent the parsed regex structure
data Regex
  = Seq [Term]        -- Sequence of terms: AB
  | Empty             -- Empty sequence (used for empty alternatives like |)
  deriving (Show, Eq)

data Term
  = Move Char         -- N, S, E, W
  | Group [Regex]     -- Group with alternatives: (A|B|C)
  deriving (Show, Eq)

-- Function to parse the regex string into the Regex data structure
parseRegex :: String -> (Regex, String)
parseRegex = parseSeq

-- Parses a sequence of terms until encountering '|', ')', or end of string
parseSeq :: String -> (Regex, String)
parseSeq input =
  let (terms, rest) = parseTerms input []
  in (if null terms then Empty else Seq (reverse terms), rest)
  where
    parseTerms :: String -> [Term] -> ([Term], String)
    parseTerms [] acc = (acc, [])
    parseTerms (c:cs) acc
      | c `elem` "NSEW" = parseTerms cs (Move c : acc)
      | c == '(' =
          let (groupTerm, rest') = parseGroup cs
          in parseTerms rest' (groupTerm : acc)
      | c `elem` "|)" = (acc, c:cs) -- Stop sequence parsing
      | otherwise = error $ "Unexpected character in sequence: " ++ [c]

-- Parses a group (...) containing alternatives separated by |
parseGroup :: String -> (Term, String)
parseGroup input = parseAlts input []
  where
    parseAlts :: String -> [Regex] -> (Term, String)
    parseAlts [] _ = error "Unmatched parenthesis"
    parseAlts (')':cs) acc = (Group (reverse acc), cs) -- End of group
    parseAlts ('|':cs) acc = -- Separator, parse next alternative
        let (nextAlt, rest') = parseSeq cs
        in parseAlts rest' (nextAlt : acc)
    parseAlts s acc = -- Start of a new alternative (or the first one)
        let (alt, rest') = parseSeq s
        in parseAlts rest' (alt : acc)

-- Function to move one step in a given direction
move :: Char -> Pos -> Pos
move 'N' (x, y) = (x, y + 1)
move 'S' (x, y) = (x, y - 1)
move 'E' (x, y) = (x + 1, y)
move 'W' (x, y) = (x - 1, y)
move _ p        = p -- Should not happen with valid input

-- Core function to walk the parsed regex and calculate distances
-- Takes the regex structure, current distance map, and set of starting positions
-- Returns the updated distance map and the set of ending positions
walk :: Regex -> M.Map Pos Int -> S.Set Pos -> (M.Map Pos Int, S.Set Pos)
walk Empty dists starts = (dists, starts) -- Empty path doesn't change anything
walk (Seq terms) dists starts = foldl' processTerm (dists, starts) terms
  where
    -- Process a single term in a sequence
    processTerm :: (M.Map Pos Int, S.Set Pos) -> Term -> (M.Map Pos Int, S.Set Pos)
    processTerm (!accDists, !currentStarts) term = case term of
      Move m -> S.foldl' (\(!dMap, !newEnds) pos ->
                            let !nextPos = move m pos
                                !currentDist = M.findWithDefault (error "Position not found") pos dMap
                                !newDist = currentDist + 1
                                !updatedDMap = M.insertWith min nextPos newDist dMap
                            in (updatedDMap, S.insert nextPos newEnds)
                         ) (accDists, S.empty) currentStarts
      Group alts ->
          -- For each alternative, walk it starting from the *same* set of 'currentStarts'
          let results = map (\alt -> walk alt accDists currentStarts) alts
              -- Merge results: Combine all distance maps and union all end positions
              finalDists = foldl' (\d1 (d2, _) -> M.unionWith min d1 d2) accDists results
              finalEnds = foldl' (\s1 (_, s2) -> S.union s1 s2) S.empty results
          in (finalDists, finalEnds)

-- Helper because foldl' is strict in the accumulator only
foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f z [] = z
foldl' f z (x:xs) = let z' = f z x in z' `seq` foldl' f z' xs

-- Main solver function
solve :: String -> (Int, Int)
solve input =
  let regexContent = init (tail input) -- Remove ^ and $
      (parsedRegex, rest) = parseRegex regexContent
  in if not (null rest)
     then error $ "Parsing did not consume full string. Remaining: " ++ rest
     else
       let initialDistMap = M.singleton (0, 0) 0
           initialStarts = S.singleton (0, 0)
           (finalDistMap, _) = walk parsedRegex initialDistMap initialStarts

           -- Part 1: Find the maximum distance
           maxDist = maximum (M.elems finalDistMap)

           -- Part 2: Count rooms with distance >= 1000
           countDist1000 = M.size $ M.filter (>= 1000) finalDistMap

       in (maxDist, countDist1000)

-- Main entry point
main :: IO ()
main = do
  input <- readFile "input.txt"
  let inputLine = head (lines input) -- Assuming input is single line
  let (part1Result, part2Result) = solve inputLine
  putStrLn $ "Part 1: " ++ show part1Result
  putStrLn $ "Part 2: " ++ show part2Result
