
import Data.List (transpose)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)

type Tile = [String]

rotate :: Tile -> Tile
rotate = transpose . reverse

flipH :: Tile -> Tile
flipH = map reverse

borders :: Tile -> [String]
borders t = [head t, last t, map head t, map last t]

parse :: String -> [(Int, Tile)]
parse = go . lines
  where
    go [] = []
    go (l:ls)
      | "Tile" `isPrefixOf` l = let n = read (takeWhile (/= ':') (drop 5 l))
                                in let (tile, rest) = break (null . trim) ls
                                   in (n, takeWhile (not . null) tile) : go (dropWhile null rest)
      | otherwise = go ls
    isPrefixOf pre str = take (length pre) str == pre
    trim = dropWhile (== ' ')

main :: IO ()
main = do
  input <- readFile "input.txt"
  let tiles = parse input
      allBorders = concatMap (\(i, t) -> let bs = borders t in bs ++ map reverse bs) tiles
      freq = Map.fromListWith (+) [(b, 1) | b <- allBorders]
      isUnique b = Map.findWithDefault 0 b freq == 1
      cornerIds = [i | (i, t) <- tiles, length (filter isUnique (borders t)) == 2]
  print (product cornerIds)
