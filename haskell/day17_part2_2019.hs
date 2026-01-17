
import qualified Data.Map.Strict as M
import Data.Char (ord, chr)
import Data.List (intercalate, isPrefixOf)
import Control.Applicative ((<|>))

-- Intcode Machine implementation
type Memory = M.Map Int Int
data State = Running Int Int Memory [Int] [Int] | Halted [Int]

run :: State -> State
run (Halted out) = Halted out
run (Running pc rb mem input out) =
    let get addr = M.findWithDefault 0 addr mem
        set addr val m = M.insert addr val m
        mode i = (get pc `div` (10 ^ (i + 1))) `mod` 10
        param i = let v = get (pc + i) in case mode i of
            0 -> get v
            1 -> v
            2 -> get (rb + v)
            _ -> error "Unknown mode"
        addr i = let v = get (pc + i) in case mode i of
            0 -> v
            2 -> rb + v
            _ -> error "Unknown mode"
        op = get pc `mod` 100
    in case op of
        1 -> run $ Running (pc+4) rb (set (addr 3) (param 1 + param 2) mem) input out
        2 -> run $ Running (pc+4) rb (set (addr 3) (param 1 * param 2) mem) input out
        3 -> case input of
            (i:is) -> run $ Running (pc+2) rb (set (addr 1) i mem) is out
            []     -> Running pc rb mem [] out -- Suspend for input
        4 -> run $ Running (pc+2) rb mem input (out ++ [param 1])
        5 -> run $ Running (if param 1 /= 0 then param 2 else pc + 3) rb mem input out
        6 -> run $ Running (if param 1 == 0 then param 2 else pc + 3) rb mem input out
        7 -> run $ Running (pc+4) rb (set (addr 3) (if param 1 < param 2 then 1 else 0) mem) input out
        8 -> run $ Running (pc+4) rb (set (addr 3) (if param 1 == param 2 then 1 else 0) mem) input out
        9 -> run $ Running (pc+2) (rb + param 1) mem input out
        99 -> Halted out
        _ -> error "Halt"

-- Part 1: Grid parsing and Calibration
part1 :: [String] -> Int
part1 grid = sum [ r * c | r <- [1..h-2], c <- [1..w-2], isInter r c ]
  where
    h = length grid
    w = length (head grid)
    isInter r c = all (== '#') [grid !! r !! c, grid !! (r-1) !! c, grid !! (r+1) !! c, grid !! r !! (c-1), grid !! r !! (c+1)]

-- Part 2: Path generation
findStart :: [String] -> (Int, Int, Int)
findStart grid = head [ (r, c, d) | r <- [0..h-1], c <- [0..w-1], 
                        let char = grid !! r !! c, (d, s) <- zip [0..] "^>v<", char == s ]
  where h = length grid; w = length (head grid)

getPath :: [String] -> (Int, Int, Int) -> [String]
getPath grid (y, x, d) = go y x d
  where
    h = length grid
    w = length (head grid)
    at r c = if r >= 0 && r < h && c >= 0 && c < w then grid !! r !! c else '.'
    nr dir = case dir of 0 -> -1; 1 -> 0; 2 -> 1; 3 -> 0; _ -> 0
    nc dir = case dir of 0 -> 0; 1 -> 1; 2 -> 0; 3 -> -1; _ -> 0
    walk r c dir dist
        | at (r + nr dir) (c + nc dir) == '#' = walk (r + nr dir) (c + nc dir) dir (dist + 1)
        | otherwise = (dist, r, c)
    go r c dir
        | at (r + nr ((dir-1) `mod` 4)) (c + nc ((dir-1) `mod` 4)) == '#' = 
            let d' = (dir-1) `mod` 4; (dist, nr', nc') = walk r c d' 0 in "L" : show dist : go nr' nc' d'
        | at (r + nr ((dir+1) `mod` 4)) (c + nc ((dir+1) `mod` 4)) == '#' = 
            let d' = (dir+1) `mod` 4; (dist, nr', nc') = walk r c d' 0 in "R" : show dist : go nr' nc' d'
        | otherwise = []

-- Recursive Path Compression
compress :: [String] -> Maybe ([String], [[String]])
compress path = search path []
  where
    search p funcs
        | all (`elem` ["A", "B", "C"]) p = if length (intercalate "," p) <= 20 then Just (p, funcs) else Nothing
        | length funcs < 3 = 
            let start = head $ dropWhile (`elem` ["A", "B", "C"]) p
                idx = head [ i | (v, i) <- zip p [0..], v == start ]
                candidates = [ take len (drop idx p) | len <- [1..10] ]
            in foldr (<|>) Nothing $ map (tryCandidate p funcs) candidates
        | otherwise = Nothing

    tryCandidate p funcs cand
        | any (`elem` ["A", "B", "C"]) cand || length (intercalate "," cand) > 20 = Nothing
        | otherwise = search (replace cand (["A", "B", "C"] !! length funcs) p) (funcs ++ [cand])

    replace _ _ [] = []
    replace sub repl list
        | sub `isPrefixOf` list = repl : replace sub repl (drop (length sub) list)
        | otherwise = head list : replace sub repl (tail list)

main :: IO ()
main = do
    content <- readFile "input.txt"
    let memory = M.fromList $ zip [0..] (map read $ words [if c == ',' then ' ' else c | c <- content])
    
    -- Part 1 execution
    let (Halted out1) = run (Running 0 0 memory [] [])
    let grid = filter (not . null) $ lines $ map chr out1
    putStrLn $ "Part 1: " ++ show (part1 grid)
    
    -- Part 2 pathfinding and interaction
    let path = getPath grid (findStart grid)
    let (mainProg, funcs) = case compress path of
            Just res -> res
            Nothing  -> error "Compression failed"
    
    let format line = map ord (intercalate "," line ++ "\n")
    let inputs = format mainProg ++ concatMap format funcs ++ map ord "n\n"
    
    -- Part 2 execution
    let memory2 = M.insert 0 2 memory
    let (Halted out2) = run (Running 0 0 memory2 inputs [])
    putStrLn $ "Part 2: " ++ show (last out2)

