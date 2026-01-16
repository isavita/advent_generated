
import System.IO
import Data.List
import qualified Data.Set as S

sizeX,sizeY :: Int
sizeX = 101
sizeY = 103

mod' a b = ((a `mod` b) + b) `mod` b

parseLine l = (read x,read y,read vx,read vy)
  where (p,v) = break (== ' ') l
        (x:y:_) = split ',' (drop 2 p)
        (vx:vy:_) = split ',' (drop 3 v)
        split c s = case break (==c) s of (a,b) -> a : if null b then [] else split c (tail b)

move rs = [(mod' (x+vx) sizeX,mod' (y+vy) sizeY,vx,vy) | (x,y,vx,vy) <- rs]

quadrants rs = foldl' upd (1,1,1,1) rs
  where cx = sizeX `div` 2; cy = sizeY `div` 2
        upd (a,b,c,d) (x,y,_,_)
          | x < cx && y < cy = (a*2,b,c,d)
          | x < cx && y > cy = (a,b*2,c,d)
          | x > cx && y < cy = (a,b,c*2,d)
          | x > cx && y > cy = (a,b,c,d*2)
          | otherwise        = (a,b,c,d)

noOverlap rs = S.size (S.fromList [(x,y) | (x,y,_,_) <- rs]) == length rs

draw rs = mapM_ putStrLn rows
  where pos = S.fromList [(x,y) | (x,y,_,_) <- rs]
        rows = [[if S.member (x,y) pos then '#' else '.' | x <- [0..sizeX-1]] | y <- [0..sizeY-1]]

loop rs n
  | noOverlap rs = (n,rs)
  | n > 1000000  = error "Exceeded maximum iterations"
  | otherwise    = loop (move rs) (n+1)

main = do
  txt <- readFile "input.txt"
  let robots = map parseLine . filter (not . null) . lines $ txt
      part1 = quadrants (iterate (move) robots !! 100)
      safety = product (let (a,b,c,d)=part1 in [a,b,c,d])
  putStrLn $ "Part 1 - Safety Factor after 100 seconds: " ++ show safety
  let (sec,final) = loop robots 0
  putStrLn $ "Part 2 - Fewest seconds to display Easter egg: " ++ show sec
  putStrLn "Final positions of robots:"
  draw final
