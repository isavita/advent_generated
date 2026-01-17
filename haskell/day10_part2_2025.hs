
import System.IO
import Data.List
import Data.Char
import Data.Maybe

getNums :: String -> [Int]
getNums "" = []
getNums s = case dropWhile (not . isDigit) s of
    "" -> []
    s' -> let (w, rest) = span isDigit s' in read w : getNums rest

splitByParens :: String -> [String]
splitByParens "" = []
splitByParens s =
    let rest1 = dropWhile (/= '(') s
    in if null rest1 then []
       else let (inside, rest2) = span (/= ')') (tail rest1)
            in inside : splitByParens (if null rest2 then "" else tail rest2)

parseLine :: String -> ([[Int]], [Int])
parseLine s =
    let (btnsPart, targetsPart) = span (/= '{') s
        btns = map getNums (splitByParens btnsPart)
        targets = getNums targetsPart
    in (btns, targets)

swap l i j | i == j = l
           | otherwise =
    let rI = l !! i; rJ = l !! j
    in [ if k == i then rJ else if k == j then rI else row | (k, row) <- zip [0..] l ]

gauss :: Int -> Int -> [[Double]] -> ([[Double]], [Int])
gauss numRows numCols mat = step 0 0 mat []
  where
    step r c m pivots
      | r >= numRows || c >= numCols = (m, reverse pivots)
      | otherwise =
          case find (\i -> abs (m !! i !! c) > 1e-9) [r..numRows-1] of
            Nothing -> step r (c + 1) m pivots
            Just pRow ->
              let m1 = swap m r pRow
                  pVal = m1 !! r !! c
                  rowR = map (/ pVal) (m1 !! r)
                  m2 = [ if i == r then rowR
                         else zipWith (\x y -> x - (m1 !! i !! c) * y) (m1 !! i) rowR
                       | i <- [0..numRows-1] ]
              in step (r + 1) (c + 1) m2 (c : pivots)

solveLine :: String -> Int
solveLine line =
    let (btns, targets) = parseLine line
        numCounters = length targets
        numButtons = length btns
        initMat = [ [ if any (==j) (btns !! i) then 1.0 else 0.0 | i <- [0..numButtons-1] ] ++ [fromIntegral (targets !! j)] | j <- [0..numCounters-1] ]
        (rrefMat, pivotCols) = gauss numCounters numButtons initMat
        rank = length pivotCols
        isInconsistent = any (\r -> abs (rrefMat !! r !! numButtons) > 1e-9) [rank..numCounters-1]
    in if isInconsistent then -1 else 
       let freeVars = [0..numButtons-1] \\ pivotCols
           maxPs = [ let vts = [ targets !! c | c <- btn, c < numCounters ]
                     in if null vts then 0 else minimum vts | btn <- btns ]
           sortedFreeVars = sortOn (\fv -> maxPs !! fv) freeVars
           numFree = length sortedFreeVars
           pivotRowInfo = [ (pCol, row !! numButtons, [ (i, row !! fv) | (i, fv) <- zip [0..] sortedFreeVars, abs (row !! fv) > 1e-9 ])
                          | (r, pCol) <- zip [0..] pivotCols, let row = rrefMat !! r ]
           
           dfs idx currentSum fvals best
               | currentSum >= best = best
               | idx == numFree =
                   let getV i = fvals !! (numFree - 1 - i)
                       check (pCol, target, coeffs) =
                           let val = target - sum [ cv * fromIntegral (getV i) | (i, cv) <- coeffs ]
                               intVal = round val
                           in if abs (val - fromIntegral intVal) < 1e-6 && intVal >= 0 && intVal <= (maxPs !! pCol)
                              then Just intVal
                              else Nothing
                       pVals = mapM check pivotRowInfo
                   in case pVals of
                       Just vs -> min best (currentSum + sum vs)
                       Nothing -> best
               | otherwise =
                   let fv = sortedFreeVars !! idx
                       mx = maxPs !! fv
                   in foldl' (\b v -> dfs (idx + 1) (currentSum + v) (v:fvals) b) best [0..mx]
           
           res = dfs 0 0 [] 1000000000
       in if res == 1000000000 then -1 else res

main :: IO ()
main = do
    content <- readFile "input.txt"
    let ls = filter (not . null) (lines content)
    let results = map solveLine ls
    print $ sum [ r | r <- results, r > 0 ]

