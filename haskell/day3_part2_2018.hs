import qualified Data.Map as Map
import Data.List (find)
import Data.Maybe (fromJust)

main = do
    contents <- readFile "input.txt"
    let claims = map parseClaim (lines contents)
        fabric = foldl updateFabric Map.empty claims
        intactClaim = findIntactClaim claims fabric
    print intactClaim

data Claim = Claim { claimId :: Int, left :: Int, top :: Int, width :: Int, height :: Int }

parseClaim :: String -> Claim
parseClaim s = let [id, _, pos, size] = words s
                   [left, top] = map read (split ',' (init pos))
                   [width, height] = map read (split 'x' size)
               in Claim (read (tail id)) left top width height

split :: Char -> String -> [String]
split _ [] = [""]
split delim (c:cs)
    | c == delim = "" : rest
    | otherwise = (c : head rest) : tail rest
  where
    rest = split delim cs

updateFabric :: Map.Map (Int, Int) Int -> Claim -> Map.Map (Int, Int) Int
updateFabric fabric (Claim _ left top width height) =
    foldl (\f (x, y) -> Map.insertWith (+) (x, y) 1 f) fabric [(x, y) | x <- [left..left+width-1], y <- [top..top+height-1]]

findIntactClaim :: [Claim] -> Map.Map (Int, Int) Int -> Int
findIntactClaim claims fabric = claimId $ fromJust $ find (\(Claim id left top width height) -> all (\(x, y) -> Map.findWithDefault 0 (x, y) fabric == 1) [(x, y) | x <- [left..left+width-1], y <- [top..top+height-1]]) claims