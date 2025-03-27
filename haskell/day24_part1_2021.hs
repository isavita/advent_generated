
import qualified Data.Map.Strict as M
import Data.List (foldl')
import Data.Array
import System.IO (readFile)

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let ls = lines contents
        indexedLines = zip [0..] ls

    let parseLastInt line = read (last (words line)) :: Int

    let l = [ parseLastInt line | (i, line) <- indexedLines, i `mod` 18 == 4 ]
    let k = [ parseLastInt line | (i, line) <- indexedLines, i `mod` 18 == 5 ]
    let m = [ parseLastInt line | (i, line) <- indexedLines, i `mod` 18 == 15 ]

    let initialState = ([], M.empty :: M.Map Int (Int, Int))
    let processInstruction (stack, constraints) (i, l_val) =
            if l_val == 1 then
                (i : stack, constraints)
            else
                case stack of
                    (pop : rest) ->
                        let constraintVal = m !! pop + k !! i
                        in (rest, M.insert pop (i, constraintVal) constraints)
                    [] -> error "Stack underflow processing input"

    let (_, finalConstraints) = foldl' processInstruction initialState (zip [0..] l)

    let bounds = (0, 13) :: (Int, Int)
    let initialArray = listArray bounds (replicate 14 0) :: Array Int Int

    let updates = concatMap calculatePair (M.toList finalConstraints)
          where
            calculatePair :: (Int, (Int, Int)) -> [(Int, Int)]
            calculatePair (i, (j, diff)) =
                let calculateVmax d = if d + diff > 9 then calculateVmax (d - 1) else d
                    vmax = calculateVmax 9
                in [(i, vmax), (j, vmax + diff)]

    let maxValsArray = initialArray // updates
    let maxVals = elems maxValsArray

    let num :: [Int] -> Integer
        num = foldl' (\acc digit -> acc * 10 + fromIntegral digit) 0

    print $ num maxVals
