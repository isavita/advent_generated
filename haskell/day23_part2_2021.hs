
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.List as L
import System.IO (readFile)
import Data.Maybe (isNothing, fromJust)

type Pos = (Int, Int)
type Grid = M.Map Pos Char

targetCols :: M.Map Char Int
targetCols = M.fromList [('A',3),('B',5),('C',7),('D',9)]

costs :: M.Map Char Int
costs = M.fromList [('A',1),('B',10),('C',100),('D',1000)]

hallwayCols :: [Int]
hallwayCols = [1,2,4,6,8,10,11]

roomRows :: [Int]
roomRows = [2,3,4,5]

main :: IO ()
main = do
    content <- readFile "input.txt"
    let raw = L.filter (not . null) (lines content)
        extra = ["  #D#C#B#A#","  #D#B#A#C#"]
        full = [ raw !! 0, raw !! 1, raw !! 2 ] ++ extra ++ [ raw !! 3, raw !! 4 ]
        grid = M.fromList [ ((r,c),ch) |
                 (r,line) <- zip [0..] full,
                 (c,ch) <- zip [0..] line,
                 ch `elem` "ABCD"]
        ans = dijkstra (S.singleton (0,grid)) S.empty
    print ans

dijkstra :: S.Set (Int, Grid) -> S.Set Grid -> Int
dijkstra pq seen =
    let ((e,g),pq') = fromJust (S.minView pq)
    in if S.member g seen then dijkstra pq' seen
       else if done g then e
       else let seen' = S.insert g seen
                new = [ (e + dist * c, g')
                      | (p,t,ch) <- getMoves g
                      , let dist = manhattan p t
                            c = costs M.! ch
                            g' = M.insert t ch (M.delete p g) ]
                pq'' = L.foldl' (flip S.insert) pq' new
            in dijkstra pq'' seen'

done :: Grid -> Bool
done g = all (\(p,ch) -> settled g p ch) (M.toList g)

settled :: Grid -> Pos -> Char -> Bool
settled g (r,c) ch =
    r > 1 && c == (targetCols M.! ch) &&
    all (\rr -> rr <= r || M.lookup (rr,c) g == Just ch) roomRows

getMoves :: Grid -> [(Pos, Pos, Char)]
getMoves g =
    let toRoom = [ (p,t,ch) |
                  (p,ch) <- M.toList g,
                  not (settled g p ch),
                  Just t <- [canMoveToRoom g p ch] ]
    in if not (null toRoom) then toRoom
       else [ (p,(1,c),ch) |
              (p,ch) <- M.toList g,
              not (settled g p ch),
              let (pr,_) = p,
              pr > 1,
              c <- hallwayCols,
              pathClear g p (1,c) ]

canMoveToRoom :: Grid -> Pos -> Char -> Maybe Pos
canMoveToRoom g (r,c) ch =
    let tc = targetCols M.! ch
        ok = all (\rr -> maybe True (==ch) (M.lookup (rr,tc) g)) roomRows
    in if not ok then Nothing
       else let tr = L.find (\rr -> isNothing (M.lookup (rr,tc) g)) (reverse roomRows)
            in case tr of
                 Just rr | pathClear g (r,c) (rr,tc) -> Just (rr,tc)
                 _ -> Nothing

pathClear :: Grid -> Pos -> Pos -> Bool
pathClear g (r1,c1) (r2,c2) =
    let hRange = if c1 < c2 then [c1+1..c2] else if c1 > c2 then [c1-1,c1-2..c2] else []
        v1 = if r1 > 1 then [(rr,c1) | rr <- [1..r1-1]] else []
        h  = [(1,cc) | cc <- hRange]
        v2 = if r2 > 1 then [(rr,c2) | rr <- [2..r2]] else []
        path = filter (/= (r1,c1)) (v1 ++ h ++ v2)
    in all (\p -> isNothing (M.lookup p g)) path

manhattan :: Pos -> Pos -> Int
manhattan (r1,c1) (r2,c2) = abs (c1-c2) + (r1-1) + (r2-1)
