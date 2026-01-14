
import Data.Char (isSpace)
import qualified Data.IntSet as IS
import System.IO

main :: IO ()
main = do
    content <- readFile "input.txt"
    let cleaned = map (\c -> if isSpace c || c == ',' then ' ' else c) content
        tokens = words cleaned
        ids = IS.fromList
            [ seed * mul
            | token <- tokens
            , '-' `elem` token
            , let (aStr, _:bStr) = break (=='-') token
            , let a = read aStr :: Int
            , let b = read bStr :: Int
            , let (lo, hi) = if a <= b then (a, b) else (b, a)
            , k <- [1..10]
            , let mul = 10 ^ k + 1
            , let minSeed = 10 ^ (k - 1)
            , let maxSeed = 10 ^ k - 1
            , let sMin = max minSeed ((lo + mul - 1) `div` mul)
            , let sMax = min maxSeed (hi `div` mul)
            , sMin <= sMax
            , seed <- [sMin .. sMax]
            ]
    print $ IS.foldr (+) 0 ids
