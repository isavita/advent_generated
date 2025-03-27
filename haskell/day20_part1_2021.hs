
import Data.List (foldl')

parseInput :: String -> (String, [[Char]])
parseInput contents =
  let ls = lines contents
      algo = head ls
      image = drop 2 ls
  in (algo, image)

getPixel :: [[Char]] -> Bool -> Int -> Int -> Int
getPixel image flip r c =
  let rows = length image
      cols = if null image then 0 else length (head image)
      inBounds = r >= 0 && r < rows && c >= 0 && c < cols
      char = if inBounds
               then (image !! r) !! c
               else if flip then '#' else '.'
  in if char == '#' then 1 else 0

calculateIndex :: [[Char]] -> Bool -> Int -> Int -> Int
calculateIndex image flip r c =
  foldl' (\acc (dr, dc) -> acc * 2 + getPixel image flip (r + dr) (c + dc)) 0
    [(dr, dc) | dr <- [-1..1], dc <- [-1..1]]

applyAlgorithm :: [[Char]] -> String -> Bool -> [[Char]]
applyAlgorithm image algo flipInfinite =
  let oldRows = length image
      oldCols = if null image then 0 else length (head image)
      newRows = oldRows + 2
      newCols = oldCols + 2
  in [[ newChar r c | c <- [-1 .. oldCols] ] | r <- [-1 .. oldRows] ]
  where
    newChar r c =
      let idx = calculateIndex image flipInfinite r c
      in algo !! idx

enhanceImage :: [[Char]] -> String -> Int -> [[Char]]
enhanceImage initialImage algo times =
    foldl' step initialImage [0 .. times - 1]
  where
    step currentImage iter =
        let flipInfinite = head algo == '#' && odd iter
        in applyAlgorithm currentImage algo flipInfinite

countLitPixels :: [[Char]] -> Int
countLitPixels = length . filter (=='#') . concat

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let (algo, initialImage) = parseInput contents
      finalImage = enhanceImage initialImage algo 2
      litCount = countLitPixels finalImage
  print litCount

