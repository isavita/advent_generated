import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.List (transpose, isPrefixOf, find)
import Control.Applicative ((<|>))

type Point = (Int, Int, Int)

rots :: Point -> [Point]
rots (x,y,z) = 
  [ (x,y,z), (x,-z,y), (x,-y,-z), (x,z,-y)
  , (-x,-y,z), (-x,z,y), (-x,y,-z), (-x,-z,-y)
  , (y,-x,z), (y,z,x), (y,x,-z), (y,-z,-x)
  , (-y,x,z), (-y,-z,x), (-y,-x,-z), (-y,z,-x)
  , (z,y,-x), (z,x,y), (z,-y,x), (z,-x,-y)
  , (-z,-y,-x), (-z,x,-y), (-z,y,x), (-z,-x,y) ]

allOrientations :: [Point] -> [[Point]]
allOrientations pts = transpose [rots p | p <- pts]

matchOrientations :: Set.Set Point -> [Point] -> Maybe (Point, Set.Set Point)
matchOrientations settledSet rotated =
    let sList = Set.toList settledSet
        offsets = [ (sx-rx, sy-ry, sz-rz) | (sx,sy,sz) <- sList, (rx,ry,rz) <- rotated ]
        counts = Map.fromListWith (+) [(o, 1) | o <- offsets]
    in case find (\(_, count) -> count >= 12) (Map.toList counts) of
        Just (o@(ox,oy,oz), _) -> 
            Just (o, Set.fromList [(rx+ox, ry+oy, rz+oz) | (rx,ry,rz) <- rotated])
        _ -> Nothing

partitionMatches :: Set.Set Point -> [[[Point]]] -> ([(Point, Set.Set Point)], [[[Point]]])
partitionMatches qBeacons = go [] []
  where
    go matched stillUnsettled [] = (matched, stillUnsettled)
    go matched stillUnsettled (u:us) =
        case foldr (\rot acc -> acc <|> matchOrientations qBeacons rot) Nothing u of
            Just res -> go (res:matched) stillUnsettled us
            Nothing  -> go matched (u:stillUnsettled) us

solve :: [Point] -> [Set.Set Point] -> [[[Point]]] -> [Point]
solve centers queue unsettled =
    case queue of
        [] -> centers
        (qBeacons:qs) ->
            let (newlySettled, stillUnsettled) = partitionMatches qBeacons unsettled
            in solve (map fst newlySettled ++ centers) (qs ++ map snd newlySettled) stillUnsettled

parse :: String -> [[Point]]
parse content = split2 (filter (/= '\r') content)
  where
    parseScanner s = map parsePoint (tail (lines s))
    parsePoint s = let [x,y,z] = map read (splitOnComma s) in (x,y,z)
    splitOnComma s = case break (== ',') s of { (a, ',':b) -> a : splitOnComma b; (a, _) -> [a] }
    split2 "" = []
    split2 s = case breakS "\n\n" s of { (a, "") -> [parseScanner a]; (a, b) -> parseScanner a : split2 (drop 2 b) }
    breakS pat s@(x:xs) | pat `isPrefixOf` s = ([], s) | otherwise = let (ys, zs) = breakS pat xs in (x:ys, zs)
    breakS _ [] = ([], [])

main :: IO ()
main = do
    content <- readFile "input.txt"
    let scanners = parse content
    let centers = solve [(0,0,0)] [Set.fromList (head scanners)] (map allOrientations (tail scanners))
    print $ maximum [abs (x1-x2) + abs (y1-y2) + abs (z1-z2) | (x1,y1,z1) <- centers, (x2,y2,z2) <- centers]