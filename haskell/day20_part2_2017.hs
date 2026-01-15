import qualified Data.Map.Strict as Map
import System.IO

type Vec = (Int,Int,Int)
type Particle = (Vec,Vec,Vec)

parseLine :: String -> Particle
parseLine s = let ws = words $ map (\c -> if c == '-' || c `elem` ['0'..'9'] then c else ' ') s
                  [x1,y1,z1,x2,y2,z2,x3,y3,z3] = map read ws
               in ((x1,y1,z1),(x2,y2,z2),(x3,y3,z3))

move :: Particle -> Particle
move ((px,py,pz),(vx,vy,vz),(ax,ay,az)) =
  let vx' = vx + ax
      vy' = vy + ay
      vz' = vz + az
      px' = px + vx'
      py' = py + vy'
      pz' = pz + vz'
   in ((px',py',pz'),(vx',vy',vz'),(ax,ay,az))

step :: [Particle] -> [Particle]
step ps = filter (\p -> Map.findWithDefault 0 (pos p) cnt == 1) moved
  where
    moved = map move ps
    cnt = foldr (\p m -> Map.insertWith (+) (pos p) 1 m) Map.empty moved
    pos (p,_,_) = p

solve :: [String] -> Int
solve = length . (!! 1000) . iterate step . map parseLine

main :: IO ()
main = do
  content <- readFile "input.txt"
  print $ solve (lines content)