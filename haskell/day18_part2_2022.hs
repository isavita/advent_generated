
import Data.List (foldl')
import qualified Data.Set as S
import Data.Maybe (fromMaybe)

type Pt3 = (Int, Int, Int)

add :: Pt3 -> Pt3 -> Pt3
add (x1, y1, z1) (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2)

neighbors :: [Pt3]
neighbors = [(-1, 0, 0), (1, 0, 0), (0, -1, 0), (0, 1, 0), (0, 0, -1), (0, 0, 1)]

minPt :: Pt3 -> Pt3 -> Pt3
minPt (x1, y1, z1) (x2, y2, z2) = (min x1 x2, min y1 y2, min z1 z2)

maxPt :: Pt3 -> Pt3 -> Pt3
maxPt (x1, y1, z1) (x2, y2, z2) = (max x1 x2, max y1 y2, max z1 z2)

parsePt :: String -> Pt3
parsePt s = case map read (words $ map (\c -> if c == ',' then ' ' else c) s) of
    [x, y, z] -> (x, y, z)
    _ -> error "Invalid input"

solve :: [String] -> Int
solve input =
    let cubes = S.fromList $ map parsePt input
        (minP, maxP) = foldl' (\(mn, mx) p -> (minPt mn p, maxPt mx p)) ((maxBound, maxBound, maxBound), (minBound, minBound, minBound)) $ S.toList cubes
        min' = add minP (-1, -1, -1)
        max' = add maxP (1, 1, 1)
        
        bfs :: S.Set Pt3 -> [Pt3] -> S.Set Pt3 -> Int -> Int
        bfs cubes q seen faces
          | null q = faces
          | otherwise =
            let curr = head q
                q' = tail q
                (newQ, newSeen, newFaces) = foldl' (\(accQ, accSeen, accFaces) delta ->
                    let next = add curr delta
                    in if not (inBounds next min' max') then (accQ, accSeen, accFaces)
                       else if S.member next cubes then (accQ, accSeen, accFaces + 1)
                       else if S.member next accSeen then (accQ, accSeen, accFaces)
                       else (accQ ++ [next], S.insert next accSeen, accFaces)
                    ) ([], seen, faces) neighbors
            in bfs cubes (q' ++ newQ) newSeen newFaces
    in bfs cubes [min'] (S.singleton min') 0

inBounds :: Pt3 -> Pt3 -> Pt3 -> Bool
inBounds (x, y, z) (minX, minY, minZ) (maxX, maxY, maxZ) =
    x >= minX && y >= minY && z >= minZ && x <= maxX && y <= maxY && z <= maxZ

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let inputLines = lines contents
    print $ solve inputLines
