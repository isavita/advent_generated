
import Data.List (transpose)
import qualified Data.Map as M

type Coord = (Int, Int)
type Grid = M.Map Coord Char

shiftNorth :: Grid -> Int -> Int -> Grid
shiftNorth grid width height = foldl shiftRock grid [(x,y) | x <- [0..width-1], y <- [0..height-1]]
  where
    shiftRock g c@(x,y)
      | M.lookup c g == Just 'O' = shift g c
      | otherwise = g
    shift g c@(x,y) =
      case findEmpty g c of
        Just newPos -> M.insert newPos 'O' (M.delete c g)
        Nothing -> g
    findEmpty g c@(x,y) = findEmpty' g c (x,y-1)
    findEmpty' g original@(ox,oy) (x,y)
      | y < 0 = Nothing
      | M.member (x,y) g = Nothing
      | otherwise = case findEmpty' g original (x,y-1) of
                      Just p -> Just p
                      Nothing -> Just (x,y)

calculateLoad :: Grid -> Int -> Int -> Int
calculateLoad grid width height = sum $ map (\(x,y) -> height - y) rocks
  where
    rocks = filter (\c -> M.lookup c grid == Just 'O') [(x,y) | x <- [0..width-1], y <- [0..height-1]]

solve :: [String] -> Int
solve input = let
  height = length input
  width = length (head input)
  grid = M.fromList [((x,y), cell) | y <- [0..height-1], x <- [0..width-1], let cell = (input !! y) !! x, cell /= '.']
  shiftedGrid = shiftNorth grid width height
  in calculateLoad shiftedGrid width height

main :: IO ()
main = do
  input <- lines <$> readFile "input.txt"
  print (solve input)
