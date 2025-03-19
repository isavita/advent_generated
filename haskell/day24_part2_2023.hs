
import Data.Ratio
import Data.List (foldl')

data RatVec3 = RatVec3 { x :: Rational, y :: Rational, z :: Rational } deriving Show
data HailStone = HailStone { p :: RatVec3, v :: RatVec3 } deriving Show

add :: RatVec3 -> RatVec3 -> RatVec3
add (RatVec3 x1 y1 z1) (RatVec3 x2 y2 z2) = RatVec3 (x1 + x2) (y1 + y2) (z1 + z2)

sub :: RatVec3 -> RatVec3 -> RatVec3
sub (RatVec3 x1 y1 z1) (RatVec3 x2 y2 z2) = RatVec3 (x1 - x2) (y1 - y2) (z1 - z2)

mul :: RatVec3 -> Rational -> RatVec3
mul (RatVec3 x y z) s = RatVec3 (x * s) (y * s) (z * s)

cross :: RatVec3 -> RatVec3 -> RatVec3
cross (RatVec3 x1 y1 z1) (RatVec3 x2 y2 z2) = RatVec3 (y1 * z2 - z1 * y2) (z1 * x2 - x1 * z2) (x1 * y2 - y1 * x2)

dot :: RatVec3 -> RatVec3 -> Rational
dot (RatVec3 x1 y1 z1) (RatVec3 x2 y2 z2) = x1 * x2 + y1 * y2 + z1 * z2

hailStoneSub :: HailStone -> HailStone -> HailStone
hailStoneSub (HailStone p1 v1) (HailStone p2 v2) = HailStone (sub p1 p2) (sub v1 v2)

intersectionTime :: HailStone -> HailStone -> Rational
intersectionTime r s = -(dot (p s) plane) / (dot (v s) plane)
  where plane = cross (p r) (add (p r) (v r))

readLine :: String -> HailStone
readLine line = HailStone (RatVec3 x y z) (RatVec3 vx vy vz)
  where
    nums = map (toRational . read) $ words $ map (\c -> if c == ',' || c == '@' then ' ' else c) line
    [x, y, z, vx, vy, vz] = nums

readInput :: [String] -> [HailStone]
readInput = map readLine

solve :: [String] -> String
solve input = show $ numerator (x rp + y rp + z rp)
    where
      hailStones = readInput (take 3 input)
      s1 = hailStones !! 1
      s2 = hailStones !! 2
      ref1 = hailStoneSub s1 (head hailStones)
      ref2 = hailStoneSub s2 (head hailStones)
      t1 = intersectionTime ref2 ref1
      t2 = intersectionTime ref1 ref2
      rock1 = add (p s1) (mul (v s1) t1)
      rock2 = add (p s2) (mul (v s2) t2)
      rp = sub rock1 (mul (sub rock2 rock1) (t1 / (t2 - t1)))

main :: IO ()
main = do
  input <- readFile "input.txt"
  putStrLn $ solve (lines input)
