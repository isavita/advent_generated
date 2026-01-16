
import qualified Data.IntMap.Strict as IM
import Data.IntMap.Strict (IntMap)
import Data.List (foldl')
import Data.Maybe (listToMaybe)
import System.IO

main :: IO ()
main = do
    content <- readFile "input.txt"
    let rawLines = lines content
        linesNoBlank = filter (not . all (`elem` " \t\r")) rawLines
    if null linesNoBlank
        then print (0 :: Int)
        else do
            let grid = linesNoBlank
                h = length grid
                w = if null grid then 0 else length (head grid)
                startPos = listToMaybe
                    [ (x, y)
                    | (y, row) <- zip [0 ..] grid
                    , (x, ch)  <- zip [0 ..] row
                    , ch == 'S'
                    ]
            case startPos of
                Nothing -> print (0 :: Int)
                Just (sx, sy) -> do
                    let initial = IM.singleton sx 1
                        rowsFromS = drop sy grid
                        finalCounts = foldl' step initial rowsFromS
                        answer = sum (IM.elems finalCounts)
                    print answer
  where
    step :: IntMap Int -> String -> IntMap Int
    step cur row = IM.foldlWithKey' update IM.empty cur
      where
        update :: IntMap Int -> Int -> Int -> IntMap Int
        update nxt x cnt
            | 0 <= x && x < length row && row !! x == '^' =
                let nxt1 = IM.insertWith (+) (x - 1) cnt nxt
                in IM.insertWith (+) (x + 1) cnt nxt1
            | otherwise = IM.insertWith (+) x cnt nxt
