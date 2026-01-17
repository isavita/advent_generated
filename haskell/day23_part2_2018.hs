
import Data.List (foldl', foldl1')
import Data.Char (isDigit)

data Coord = Coord !Int !Int !Int deriving (Eq, Ord, Show)
data Bot = Bot !Coord !Int deriving (Eq, Show)

dist :: Coord -> Coord -> Int
dist (Coord x1 y1 z1) (Coord x2 y2 z2) = abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2)

parseBot :: String -> Bot
parseBot s = Bot (Coord (nums !! 0) (nums !! 1) (nums !! 2)) (nums !! 3)
  where nums = map read $ words [if isDigit c || c == '-' then c else ' ' | c <- s]

countInRange :: [Bot] -> Int -> Coord -> Int
countInRange bots zoom (Coord x y z) = foldl' check 0 bots
  where
    check !acc (Bot (Coord bx by bz) r) =
        let d = abs (x - (bx `quot` zoom)) +
                abs (y - (by `quot` zoom)) +
                abs (z - (bz `quot` zoom))
        in if d <= (r `quot` zoom) then acc + 1 else acc

solve :: [Bot] -> Int
solve bots = loop (2^30) (Coord 0 0 0) (Coord 0 0 0)
  where
    loop zoom (Coord tlx tly tlz) (Coord brx bry brz) =
        let points = [Coord x y z | x <- [tlx..brx], y <- [tly..bry], z <- [tlz..brz]]
            scored = map (\p -> (countInRange bots zoom p, -(dist (Coord 0 0 0) p), p)) points
            (_, _, bestP@(Coord bx by bz)) = foldl1' (\acc@(c1, d1, _) y@(c2, d2, _) ->
                                            if c2 > c1 || (c2 == c1 && d2 > d1) then y else acc) scored
        in if zoom == 1
           then dist (Coord 0 0 0) bestP
           else loop (zoom `div` 2)
                     (Coord ((bx - 1) * 2) ((by - 1) * 2) ((bz - 1) * 2))
                     (Coord ((bx + 1) * 2) ((by + 1) * 2) ((bz + 1) * 2))

main :: IO ()
main = do
    input <- readFile "input.txt"
    let bots = map parseBot (lines input)
    print $ solve bots

