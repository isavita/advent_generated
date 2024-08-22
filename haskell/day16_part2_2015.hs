import System.IO
import Data.List
import Data.Maybe

data Sue = Sue { number :: Int, attributes :: [(String, Int)] } deriving Show

parseSue :: String -> Sue
parseSue line =
    let parts = words line
        sueNum = read (init (parts !! 1)) :: Int
        attrs = map (\[k, v] -> (init k, read (filter (/= ',') v) :: Int)) $ chunksOf 2 $ drop 2 parts
    in Sue sueNum attrs

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

matchesMFCSAM :: Sue -> Bool
matchesMFCSAM sue = all match (attributes sue)
    where
        match ("children", v) = v == 3
        match ("cats", v) = v > 7
        match ("samoyeds", v) = v == 2
        match ("pomeranians", v) = v < 3
        match ("akitas", v) = v == 0
        match ("vizslas", v) = v == 0
        match ("goldfish", v) = v < 5
        match ("trees", v) = v > 3
        match ("cars", v) = v == 2
        match ("perfumes", v) = v == 1
        match _ = True

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let sues = map parseSue (lines contents)
        matchingSue = fromJust $ find matchesMFCSAM sues
    print $ number matchingSue