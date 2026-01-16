
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import Data.Maybe (fromJust)
import System.IO

type Pos = (Int, Int)
type Dir = (Int, Int)

add :: Pos -> Dir -> Pos
add (x, y) (dx, dy) = (x + dx, y + dy)

parse :: String -> (Map Pos Char, [Dir])
parse s = (grid, steps)
  where
    (blk1 : blk2 : _) = splitOn "\n\n" s
    lines1 = lines blk1
    grid = M.fromList [ ((x, y), c)
                      | (y, row) <- zip [0 ..] lines1
                      , (x, c)   <- zip [0 ..] row ]
    steps = map dirFromChar $ filter (/= '\n') blk2
    dirFromChar '^' = (0, -1)
    dirFromChar 'v' = (0, 1)
    dirFromChar '<' = (-1, 0)
    dirFromChar '>' = (1, 0)
    dirFromChar _   = (0, 0)

splitOn :: Eq a => [a] -> [a] -> [[a]]
splitOn delim xs = go xs
  where
    go [] = []
    go ys = let (h, t) = breakList delim ys
           in h : case t of
                    [] -> []
                    _  -> go (drop (length delim) t)
    breakList d = breakList' []
      where
        breakList' acc [] = (reverse acc, [])
        breakList' acc rest@(z:zs)
          | d `isPrefixOf` rest = (reverse acc, rest)
          | otherwise           = breakList' (z:acc) zs
    isPrefixOf p q = take (length p) q == p

tryStep :: Map Pos Char -> Pos -> Dir -> Maybe (Map Pos Char)
tryStep m pos dir = case M.lookup pos m of
  Just '.' -> Just m
  Just 'O' -> move pos
  Just '@' -> move pos
  Just ']' -> tryStep m (add pos (-1, 0)) dir
  Just '[' -> case dir of
    (-1,0) -> do
      m' <- tryStep m (add pos (-1,0)) dir
      let m1 = M.insert (add pos (-1,0)) '[' $
               M.insert pos ']' $
               M.insert (add pos (1,0)) '.' m'
      return m1
    (1,0) -> do
      m' <- tryStep m (add pos (2,0)) dir
      let m1 = M.insert pos '.' $
               M.insert (add pos (1,0)) '[' $
               M.insert (add pos (2,0)) ']' m'
      return m1
    _ -> do
      m'  <- tryStep m (add pos dir) dir
      m'' <- tryStep m' (add (add pos (1,0)) dir) dir
      let m1 = M.insert pos '.' $
               M.insert (add pos (1,0)) '.' $
               M.insert (add pos dir) '[' $
               M.insert (add (add pos dir) (1,0)) ']' m''
      return m1
  _ -> Nothing
  where
    move p = do
      m' <- tryStep m (add p dir) dir
      let c = fromJust $ M.lookup p m'
          m1 = M.insert (add p dir) c $ M.insert p '.' m'
      return m1

solve :: String -> Int
solve s = sum [ fromIntegral x + 100 * fromIntegral y
             | ((x, y), c) <- M.toList finalMap
             , c `elem` "[O" ]
  where
    (grid0, dirs) = parse s
    robot0 = fst . fromJust $ M.foldrWithKey (\k v acc -> if v == '@' then Just (k,()) else acc) Nothing grid0
    (finalMap, _) = foldl step (grid0, robot0) dirs
    step (g, r) d = case tryStep g r d of
                     Just g' -> (g', add r d)
                     Nothing -> (g, r)

scaleUp :: String -> String
scaleUp = concatMap repl
  where
    repl '#' = "##"
    repl '.' = ".."
    repl 'O' = "[]"
    repl '@' = "@."
    repl c   = [c]

main :: IO ()
main = do
  content <- readFile "input.txt"
  print (solve content)
  print (solve (scaleUp content))
