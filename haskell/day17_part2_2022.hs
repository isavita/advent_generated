
import Data.Bits ( (.|.), (.&.), shiftL )
import qualified Data.Map.Strict as M
import System.IO ( readFile )

type Rock = [(Int, Int)]

rocks :: [Rock]
rocks = [ [(0,0), (1,0), (2,0), (3,0)]
        , [(1,0), (0,1), (1,1), (2,1), (1,2)]
        , [(0,0), (1,0), (2,0), (2,1), (2,2)]
        , [(0,0), (0,1), (0,2), (0,3)]
        , [(0,0), (1,0), (0,1), (1,1)] ]

rockHeights :: [Int]
rockHeights = [0, 2, 2, 3, 1]

collision :: M.Map Integer Int -> Rock -> (Int, Integer) -> Bool
collision grid rock (px, py) = any (\(rx, ry) -> 
    let nx = px + rx
        ny = py + fromIntegral ry
    in nx < 0 || nx > 6 || ny <= 0 || (M.findWithDefault 0 ny grid .&. (1 `shiftL` nx) /= 0)) rock

addRock :: M.Map Integer Int -> Rock -> (Int, Integer) -> M.Map Integer Int
addRock grid rock (px, py) = foldr (\(rx, ry) acc -> 
    let nx = px + rx
        ny = py + fromIntegral ry
    in M.insertWith (.|.) ny (1 `shiftL` nx) acc) grid rock

getProfile :: M.Map Integer Int -> Integer -> [Int]
getProfile grid h = [M.findWithDefault 0 y grid | y <- [h, h-1 .. h-29]]

dropRock :: M.Map Integer Int -> Rock -> (Int, Integer) -> String -> Int -> ((Int, Integer), String, Int)
dropRock grid rock (px, py) (jet:js) jIdx =
    let dx = if jet == '<' then -1 else 1
        pxJ = if collision grid rock (px + dx, py) then px else px + dx
        pyG = py - 1
    in if collision grid rock (pxJ, pyG)
       then ((pxJ, py), js, jIdx + 1)
       else dropRock grid rock (pxJ, pyG) js (jIdx + 1)

solve :: Integer -> Int -> Integer -> M.Map Integer Int -> Int -> M.Map (Int, Int, [Int]) (Integer, Integer) -> String -> Int -> Integer -> Integer
solve i j h grid rIdx memo jets nJets target
    | i == target = h
    | otherwise =
        let key = (rIdx, j `mod` nJets, getProfile grid h)
        in case M.lookup key memo of
            Just (pI, pH) | (target - i) `mod` (i - pI) == 0 -> h + ((target - i) `div` (i - pI)) * (h - pH)
            _ -> let rock = rocks !! rIdx
                     ((fx, fy), nJets', nJ) = dropRock grid rock (2, h + 4) jets j
                     newGrid = addRock grid rock (fx, fy)
                     newH = max h (fy + fromIntegral (rockHeights !! rIdx))
                 in solve (i + 1) nJ newH newGrid ((rIdx + 1) `mod` 5) (M.insert key (i, h) memo) nJets' nJets target

main :: IO ()
main = do
    input <- readFile "input.txt"
    let jetLine = filter (`elem` "<>") (head $ lines input ++ [""])
    if null jetLine then return () else do
        let nJets = length jetLine
            jets = cycle jetLine
            target = 1000000000000
        print $ solve 0 0 0 M.empty 0 M.empty jets nJets target

