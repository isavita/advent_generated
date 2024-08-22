import System.IO

data Position = Position { x :: Int, y :: Int, z :: Int } deriving Show

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let directions = wordsWhen (== ',') $ head $ lines contents
        startPosition = Position 0 0 0
        (finalPosition, maxDistance) = foldl move (startPosition, 0) directions
        distance = calculateDistance finalPosition
    print distance
    print maxDistance

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s''
        where (w, s'') = break p s'

move :: (Position, Int) -> String -> (Position, Int)
move (pos, maxDist) dir = let newPos = case dir of
                                            "n"  -> Position (x pos) (y pos + 1) (z pos - 1)
                                            "ne" -> Position (x pos + 1) (y pos) (z pos - 1)
                                            "se" -> Position (x pos + 1) (y pos - 1) (z pos)
                                            "s"  -> Position (x pos) (y pos - 1) (z pos + 1)
                                            "sw" -> Position (x pos - 1) (y pos) (z pos + 1)
                                            "nw" -> Position (x pos - 1) (y pos + 1) (z pos)
                      in (newPos, max maxDist (calculateDistance newPos))

calculateDistance :: Position -> Int
calculateDistance pos = (abs (x pos) + abs (y pos) + abs (z pos)) `div` 2