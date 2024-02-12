
import Data.List
import Data.Char

main :: IO ()
main = do
    inputData <- readFile "input.txt"
    let imageData = trim inputData
        width = 25
        height = 6
        layerSize = width * height
        (result, _) = foldl' processLayer (0, layerSize + 1) (chunksOf layerSize imageData)
    print result

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

processLayer :: (Int, Int) -> String -> (Int, Int)
processLayer (res, minZeros) layer =
    let (zeroCount, oneCount, twoCount) = foldl' countPixels (0, 0, 0) layer
    in if zeroCount < minZeros
        then (oneCount * twoCount, zeroCount)
        else (res, minZeros)

countPixels :: (Int, Int, Int) -> Char -> (Int, Int, Int)
countPixels (zeros, ones, twos) pixel
    | pixel == '0' = (zeros + 1, ones, twos)
    | pixel == '1' = (zeros, ones + 1, twos)
    | pixel == '2' = (zeros, ones, twos + 1)
    | otherwise = (zeros, ones, twos)

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)
