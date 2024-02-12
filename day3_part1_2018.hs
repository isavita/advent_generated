
import System.IO
import Data.List
import Text.Printf

data Claim = Claim { id :: Int, left :: Int, top :: Int, width :: Int, height :: Int }

parseClaim :: String -> Claim
parseClaim s = let [id, l, t, w, h] = map read $ words $ map (\c -> if c `elem` "#@,:x" then ' ' else c) s in Claim id l t w h

readClaims :: String -> IO [Claim]
readClaims filename = do
    contents <- readFile filename
    return $ map parseClaim $ lines contents

countOverlappingInches :: [Claim] -> Int
countOverlappingInches claims = length $ filter (>1) $ map length $ group $ sort [ (x, y) | claim <- claims, x <- [left claim..left claim+width claim-1], y <- [top claim..top claim+height claim-1] ]

main :: IO ()
main = do
    claims <- readClaims "input.txt"
    let overlapping = countOverlappingInches claims
    printf "%d\n" overlapping
