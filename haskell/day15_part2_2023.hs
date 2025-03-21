
import Data.Char (ord)
import Data.List (findIndex)
import Data.Maybe (fromMaybe)
import qualified Data.Map as M

hashTableSize :: Int
hashTableSize = 256

hashString :: String -> Int
hashString = foldl (\acc c -> (acc + ord c) * 17 `mod` hashTableSize) 0

data Step = Step { label :: String, numBox :: Int, operation :: Char, number :: Maybe Int } deriving (Show)

parseStep :: String -> Step
parseStep s = Step l (hashString l) op num
  where
    l = takeWhile (\c -> c `notElem` "=-0123456789") s
    op = s !! length l
    num = case drop (length l + 1) s of
            "" -> Nothing
            ns -> Just (read ns :: Int)

getBoxes :: [String] -> M.Map Int [(String, Int)]
getBoxes stepsStr = foldl processStep M.empty (map parseStep stepsStr)
  where
    processStep boxes step =
      case operation step of
        '-' -> M.adjust (filter (\(l, _) -> l /= label step)) (numBox step) boxes
        '=' ->
          let boxNum = numBox step
              lensLabel = label step
              focalLength = fromMaybe 0 (number step)
              updateBox boxContents =
                case findIndex (\(l, _) -> l == lensLabel) boxContents of
                  Just index -> let (prefix, _ : suffix) = splitAt index boxContents
                                 in prefix ++ ((lensLabel, focalLength) : suffix)
                  Nothing -> boxContents ++ [(lensLabel, focalLength)]
          in M.alter (Just . updateBox . fromMaybe []) boxNum boxes
        _ -> boxes

calculatePower :: M.Map Int [(String, Int)] -> Int
calculatePower boxes =
  sum $ map calculateBoxPower (M.toList boxes)
  where
    calculateBoxPower (boxNum, boxContents) =
      sum $ map (\(slot, (_, focalLength)) -> (boxNum + 1) * slot * focalLength) $ zip [1..] boxContents

solve :: [String] -> Int
solve input = calculatePower $ getBoxes $ head inputSplit
  where
      inputSplit = map words $ map (\s -> map (\c -> if c == ',' then ' ' else c) s) input


main :: IO ()
main = do
  input <- lines <$> readFile "input.txt"
  print $ solve input
