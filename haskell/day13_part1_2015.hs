
import Data.List (permutations, maximumBy)
import Data.Ord (comparing)
import qualified Data.Map as M
import System.IO

type HappinessMap = M.Map String (M.Map String Int)

main :: IO ()
main = do
    happinessMap <- readHappinessValues "input.txt"
    let guests = M.keys happinessMap
        maxHappiness = calculateOptimalArrangement guests happinessMap
    print maxHappiness

readHappinessValues :: FilePath -> IO HappinessMap
readHappinessValues filename = do
    content <- readFile filename
    let happinessMap = foldr parseLine M.empty (lines content)
    return happinessMap

parseLine :: String -> HappinessMap -> HappinessMap
parseLine line happinessMap =
    let words' = words line
        from = head words'
        to = init (last words')
        change = read (words' !! 3) * if words' !! 2 == "lose" then -1 else 1
    in M.insertWith (M.unionWith (+)) from (M.singleton to change) happinessMap

calculateOptimalArrangement :: [String] -> HappinessMap -> Int
calculateOptimalArrangement guests happinessMap =
    maximum $ map (calculateHappiness happinessMap) (permutations guests)

calculateHappiness :: HappinessMap -> [String] -> Int
calculateHappiness happinessMap arrangement =
    sum [happinessMap M.! guest M.! left + happinessMap M.! guest M.! right |
         (guest, i) <- zip arrangement [0..],
         let n = length arrangement,
         let left = arrangement !! ((i - 1 + n) `mod` n),
         let right = arrangement !! ((i + 1) `mod` n)]
