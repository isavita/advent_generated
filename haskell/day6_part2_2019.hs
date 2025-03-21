
import Data.List (findIndex)
import Data.Map (Map, fromList, (!?), keys)
import System.IO

orbitCount :: Map String String -> String -> Int
orbitCount orbitMap obj = case orbitMap !? obj of
  Nothing -> 0
  Just orbitee -> 1 + orbitCount orbitMap orbitee

pathToCom :: Map String String -> String -> [String]
pathToCom orbitMap obj =
  case orbitMap !? obj of
    Nothing -> []
    Just orbitee -> orbitee : pathToCom orbitMap orbitee

main :: IO ()
main = do
  input <- readFile "input.txt"
  let orbits = map ((\[a, b] -> (b, a)) . words . replace ')' ' ') $ lines input
      orbitMap = fromList orbits

  let totalOrbits = sum $ map (orbitCount orbitMap) (keys orbitMap)
  print totalOrbits

  let pathToYou = pathToCom orbitMap "YOU"
      pathToSan = pathToCom orbitMap "SAN"

      commonObj = head $ filter (`elem` pathToSan) pathToYou

      transfers =
        (case findIndex (== commonObj) pathToYou of
           Just youIndex -> youIndex
           Nothing -> error "Common object not found in YOU's path")
          + (case findIndex (== commonObj) pathToSan of
             Just sanIndex -> sanIndex
             Nothing -> error "Common object not found in SAN's path")
  print transfers

replace :: Char -> Char -> String -> String
replace old new = map (\c -> if c == old then new else c)
