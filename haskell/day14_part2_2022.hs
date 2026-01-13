
import qualified Data.Set as S
import System.IO

type Pt = (Int, Int)

main :: IO ()
main = do
    txt <- readFile "input.txt"
    let ls = filter (not . null) $ lines txt
        raw = map parseLine ls
        lowC = minimum $ map fst $ concat raw
        highR = maximum $ map snd $ concat raw
        extra = 200
        shift c = c - lowC + extra
        coord = map (map (\(c,r) -> (shift c, r))) raw
        origin = shift 500
        floorY = highR + 2
        rocks = foldl addPath S.empty coord
        ans = simulate rocks floorY origin
    print ans

parseLine :: String -> [Pt]
parseLine s =
    let f c = if c == ',' || c == '-' || c == '>' then ' ' else c
        ws = words $ map f s
    in [(read (ws !! i), read (ws !! (i+1))) | i <- [0,2..length ws - 2]]

addPath :: S.Set Pt -> [Pt] -> S.Set Pt
addPath st [] = st
addPath st [_] = st
addPath st ((x1,y1):(x2,y2):ps) =
    let rng a b = [min a b .. max a b]
        pts = if x1 == x2 then [(x1, y) | y <- rng y1 y2]
               else [(x, y1) | x <- rng x1 x2]
    in addPath (foldr S.insert st pts) ((x2,y2):ps)

simulate :: S.Set Pt -> Int -> Int -> Int
simulate rocks floorY origin = go S.empty 0
  where
    occupied s p = S.member p rocks || S.member p s
    go s cnt
      | S.member (origin,0) s = cnt
      | otherwise = case dropGrain (origin,0) s of
          Just p  -> go (S.insert p s) (cnt+1)
          Nothing -> cnt
    dropGrain (x,y) s
      | y+1 == floorY = Just (x,y)
      | not (occupied s (x, y+1)) = dropGrain (x, y+1) s
      | not (occupied s (x-1, y+1)) = dropGrain (x-1, y+1) s
      | not (occupied s (x+1, y+1)) = dropGrain (x+1, y+1) s
      | otherwise = Just (x, y)
