
import System.IO (readFile)
import Data.List (maximumBy, foldl')
import Data.Ord (comparing)

type Bot = (Int, Int, Int, Int)   -- (x,y,z,r)

main :: IO ()
main = do
    content <- readFile "input.txt"
    let bots = map parse . lines $ content
        (sx, sy, sz, sr) = strongest bots
        in print $ countInRange bots (sx, sy, sz, sr)

parse :: String -> Bot
parse s =
    let ws = words $ map repl s
        repl c = if c `elem` "pos=<>, r=" then ' ' else c
    in case ws of
        [x,y,z,r] -> (read x, read y, read z, read r)
        _         -> error "invalid input"

strongest :: [Bot] -> Bot
strongest = maximumBy (comparing (\(_,_,_,r) -> r))

countInRange :: [Bot] -> Bot -> Int
countInRange bots (sx, sy, sz, sr) =
    foldl' (\acc (x,y,z,_) -> if manhattan (x,y,z) (sx,sy,sz) <= sr then acc+1 else acc) 0 bots

manhattan :: (Int,Int,Int) -> (Int,Int,Int) -> Int
manhattan (x1,y1,z1) (x2,y2,z2) =
    abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2)
