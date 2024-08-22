import System.IO

data Disc = Disc { positions :: Int, startPos :: Int }

main = do
    contents <- readFile "input.txt"
    let discs = parseInput contents
        newDisc = Disc 11 0
        allDiscs = discs ++ [newDisc]
        time = findFirstTime allDiscs
    print time

parseInput :: String -> [Disc]
parseInput input = map parseLine (lines input)
  where
    parseLine line = let parts = words line
                         positions = read (parts !! 3)
                         startPos = read (init (parts !! 11))
                     in Disc positions startPos

findFirstTime :: [Disc] -> Int
findFirstTime discs = head [t | t <- [0..], all (capsuleFallsThrough t) (zip discs [1..])]

capsuleFallsThrough :: Int -> (Disc, Int) -> Bool
capsuleFallsThrough time (disc, index) = (startPos disc + time + index) `mod` positions disc == 0