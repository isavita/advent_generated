
import Data.List
import qualified Data.Map as Map
import System.IO

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let happinessMap = readHappinessValues contents
    let happinessMapWithYou = addYourself happinessMap
    let guests = getGuestList happinessMapWithYou
    let maxHappiness = calculateOptimalArrangement guests happinessMapWithYou
    print maxHappiness

readHappinessValues :: String -> Map.Map String (Map.Map String Int)
readHappinessValues input = Map.fromListWith (Map.union) $ map parseLine (lines input)
    where
        parseLine line = let wordsList = words line
                             from = head wordsList
                             to = init (last wordsList)
                             change = read (wordsList !! 3) :: Int
                         in (from, Map.singleton to (if wordsList !! 2 == "lose" then -change else change))

addYourself :: Map.Map String (Map.Map String Int) -> Map.Map String (Map.Map String Int)
addYourself happinessMap = foldl' (\acc guest -> Map.insert guest (Map.insert "You" 0 (acc Map.! guest)) acc) updatedMap guests
    where
        guests = Map.keys happinessMap
        updatedMap = Map.insert "You" (Map.fromList $ zip guests (repeat 0)) happinessMap

getGuestList :: Map.Map String (Map.Map String Int) -> [String]
getGuestList happinessMap = Map.keys happinessMap

calculateOptimalArrangement :: [String] -> Map.Map String (Map.Map String Int) -> Int
calculateOptimalArrangement guests happinessMap = maximum $ map (calculateHappiness happinessMap) (permutations guests)

calculateHappiness :: Map.Map String (Map.Map String Int) -> [String] -> Int
calculateHappiness happinessMap arrangement = sum $ map calculateSingleHappiness (zip arrangement (tail arrangement ++ [head arrangement]))
    where
        calculateSingleHappiness (guest1, guest2) = (happinessMap Map.! guest1) Map.! guest2 + (happinessMap Map.! guest2) Map.! guest1
