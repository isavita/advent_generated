
import qualified Data.Map.Strict as M
import Control.Monad.State.Strict
import Data.Char (isDigit)
import Control.Monad (forM_)

data Tile = Clay | Flow | Settle deriving (Eq, Show)
type Grid = M.Map (Int, Int) Tile

extractNums :: String -> [Int]
extractNums [] = []
extractNums s = case dropWhile (not . isDigit) s of
    "" -> []
    s' -> let (num, rest) = span isDigit s'
          in read num : extractNums rest

parseLine :: String -> [(Int, Int)]
parseLine s = case extractNums s of
    (a:b:c:_) -> if head s == 'x'
                 then [(a, y) | y <- [b..c]]
                 else [(x, a) | x <- [b..c]]
    _ -> []

fill :: Int -> Int -> Int -> State Grid Bool
fill x y maxY
    | y > maxY = return False
    | otherwise = do
        grid <- get
        case M.lookup (x, y) grid of
            Just Clay   -> return True
            Just Settle -> return True
            Just Flow   -> return False
            Nothing -> do
                modify $ M.insert (x, y) Flow
                downBlocked <- fill x (y + 1) maxY
                if not downBlocked then return False
                else do
                    (lW, lX) <- checkSide x y (-1) maxY
                    (rW, rX) <- checkSide x y 1 maxY
                    if lW && rW then do
                        forM_ [lX + 1 .. rX - 1] $ \ix -> modify $ M.insert (ix, y) Settle
                        return True
                    else return False

checkSide :: Int -> Int -> Int -> Int -> State Grid (Bool, Int)
checkSide x y dx maxY = do
    grid <- get
    case M.lookup (x, y) grid of
        Just Clay   -> return (True, x)
        Just Settle -> return (True, x)
        _ -> do
            modify $ M.insert (x, y) Flow
            downBlocked <- fill x (y + 1) maxY
            if downBlocked then checkSide (x + dx) y dx maxY
            else return (False, x)

main :: IO ()
main = do
    input <- readFile "input.txt"
    let clay = concatMap parseLine (lines input)
        minY = minimum (map snd clay)
        maxY = maximum (map snd clay)
        initialGrid = M.fromList [(p, Clay) | p <- clay]
        finalGrid = execState (fill 500 0 maxY) initialGrid
        isWater t = t == Flow || t == Settle
        ans = M.size $ M.filterWithKey (\(_, y) t -> y >= minY && y <= maxY && isWater t) finalGrid
    print ans

