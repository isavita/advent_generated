
import qualified Data.IntSet as IS
import System.IO

process :: Int -> [String] -> IS.IntSet -> Int -> Int
process _ [] _ s = s
process w (r:rs) a s =
  let (n,a') = IS.foldr (\i (set,c) ->
                 if r !! i == '^'
                 then ( (if i>0 then IS.insert (i-1) set else set)
                      `IS.union` (if i+1<w then IS.singleton (i+1) else IS.empty)
                      , c+1)
                 else (IS.insert i set, c)
               ) (IS.empty,0) a
  in if IS.null n then s + a' else process w rs n (s + a')

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let grid = lines contents
      h = length grid
      w = if null grid then 0 else length (head grid)
      (sy,sx) = head [ (y,x) | (y,row) <- zip [0..] grid
                             , let mx = elemIndex 'S' row
                             , Just x <- [mx] ]
      rows = drop sy grid
      start = IS.singleton sx
      ans = process w rows start 0
  print ans
  where
    elemIndex c = findIndex (==c)
    findIndex p xs = go 0 xs where
      go _ [] = Nothing
      go i (y:ys) = if p y then Just i else go (i+1) ys
