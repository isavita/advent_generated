
import Data.List
import Data.Char (isDigit)
import qualified Data.Map as M

main :: IO ()
main = do
    input <- readFile "input.txt"
    let games = lines input
    let targetCubes = M.fromList [("red", 12), ("green", 13), ("blue", 14)]
    let result = sum . map (fst) . filter (\(_, valid) -> valid) . map (parseGame targetCubes) $ games
    print result

parseGame :: M.Map String Int -> String -> (Int, Bool)
parseGame targetCubes game =
  let gameId = read . takeWhile isDigit . drop (length "Game ") $ game ::Int
      cubeSets = splitBy ';' . drop (length "Game " + length (show gameId) + 2) $ game
      isValidSet set = all (isValidCube targetCubes) (splitBy ',' set)
  in (gameId, all isValidSet cubeSets)
  
isValidCube :: M.Map String Int -> String -> Bool
isValidCube targetCubes cube =
  let (countStr, color) = break (==' ') (dropWhile (not . isDigit) cube)
      count = read countStr
  in maybe True (>= count) (M.lookup (trim color) targetCubes)

splitBy :: Char -> String -> [String]
splitBy delimiter = foldr (\c acc -> if c == delimiter then []:acc else (c:head acc):tail acc) [""]

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile (==' ')
