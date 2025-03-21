
import qualified Data.Array as A
import Data.Array ((!))
import Data.Char (isDigit)
import Data.Maybe (fromMaybe)
import qualified Data.Sequence as Seq
import Data.Sequence (Seq, (|>))
import qualified System.IO as IO

type Grid = A.Array (Int, Int) Char

type Dist = A.Array (Int, Int) Int

type Point = (Int, Int)

dirs :: [(Int, Int)]
dirs = [(1, 0), (-1, 0), (0, 1), (0, -1)]

bfs :: Grid -> Point -> Dist
bfs grid start =
  let bounds@((r0, c0), (r1, c1)) = A.bounds grid
      h = r1 - r0 + 1
      w = c1 - c0 + 1
      initialDist = A.listArray bounds (repeat (-1)) :: Dist
      q = Seq.singleton start
      dist = initialDist A.// [(start, 0)]
      bfs' dist queue =
        case Seq.viewl queue of
          Seq.EmptyL -> dist
          (r, c) Seq.:< rest ->
            let exploreNeighbors d = foldl explore d dirs
                explore d (dr, dc) =
                  let nr = r + dr
                      nc = c + dc
                      newPoint = (nr, nc)
                   in if A.inRange bounds newPoint
                        then
                          if grid ! newPoint /= '#'
                            then
                              if d ! newPoint == -1
                                then
                                  d A.// [(newPoint, d ! (r, c) + 1)]
                                else
                                  d
                            else
                              d
                        else
                          d
                newDist = exploreNeighbors dist
                enqueueNeighbors q' = foldl enqueue q' dirs
                enqueue q' (dr, dc) =
                  let nr = r + dr
                      nc = c + dc
                      newPoint = (nr, nc)
                   in if A.inRange bounds newPoint
                        then
                          if grid ! newPoint /= '#'
                            then
                              if dist ! newPoint == -1
                                then
                                  q' |> newPoint
                                else
                                  q'
                            else
                              q'
                        else
                          q'
                newQueue = enqueueNeighbors rest
             in bfs' newDist newQueue
   in bfs' dist q

isTrack :: Grid -> Point -> Bool
isTrack grid (r, c) =
  let bounds@((r0, c0), (r1, c1)) = A.bounds grid
   in A.inRange bounds (r, c) && grid ! (r, c) /= '#'

solve :: String -> Int
solve input =
  let gridLines = lines input
      h = length gridLines
      w = length (head gridLines)
      bounds = ((0, 0), (h - 1, w - 1))
      gridList = concat gridLines
      grid = A.listArray bounds gridList :: Grid
      findCell c = head [(r, col) | r <- [0 .. h - 1], col <- [0 .. w - 1], grid ! (r, col) == c]
      s = findCell 'S'
      e = findCell 'E'
      distFromS = bfs grid s
      distFromE = bfs grid e
      normalCost = distFromS ! e
      trackCells = [(r, c) | r <- [0 .. h - 1], c <- [0 .. w - 1], grid ! (r, c) /= '#']
      possibleCheats =
        length
          [ ()
            | startPos <- trackCells,
              sd <- [distFromS ! startPos],
              sd /= -1,
              (dr1, dc1) <- dirs,
              let m1r = fst startPos + dr1
                  m1c = snd startPos + dc1,
              (dr2, dc2) <- dirs,
              let m2r = m1r + dr2
                  m2c = m1c + dc2,
              isTrack grid (m2r, m2c),
              ed <- [distFromE ! (m2r, m2c)],
              ed /= -1,
              let newCost = sd + 2 + ed,
              normalCost - newCost >= 100
          ]
   in if distFromS ! e == -1 then 0 else possibleCheats

main :: IO ()
main = do
  input <- IO.readFile "input.txt"
  print (solve input)
