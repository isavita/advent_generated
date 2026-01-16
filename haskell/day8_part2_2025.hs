
import System.IO (readFile)
import Data.List (sortBy)
import Data.Function (on)
import Control.Monad (forM_, when)
import Control.Monad.ST (ST, runST)
import Data.Array.ST (STUArray, newArray, readArray, writeArray)
import Data.STRef (newSTRef, readSTRef, modifySTRef')

type Point = (Int, Int, Int)

distSq :: Point -> Point -> Int
distSq (x1,y1,z1) (x2,y2,z2) = dx*dx + dy*dy + dz*dz
  where
    dx = x1 - x2
    dy = y1 - y2
    dz = z1 - z2

find :: STUArray s Int Int -> Int -> ST s Int
find parent x = do
  p <- readArray parent x
  if p == x
    then return x
    else do
      r <- find parent p
      writeArray parent x r
      return r

union :: STUArray s Int Int -> STUArray s Int Int -> Int -> Int -> ST s Bool
union parent rank a b = do
  ra <- find parent a
  rb <- find parent b
  if ra == rb then return False else do
    raRank <- readArray rank ra
    rbRank <- readArray rank rb
    if raRank < rbRank
      then writeArray parent ra rb
      else if raRank > rbRank
        then writeArray parent rb ra
        else do
          writeArray parent rb ra
          writeArray rank ra (raRank + 1)
    return True

connectAll :: [Point] -> Maybe (Point, Point)
connectAll pts = runST $ do
  let n = length pts
  parent <- newArray (0, n-1) 0 :: ST s (STUArray s Int Int)
  rank   <- newArray (0, n-1) 0 :: ST s (STUArray s Int Int)
  forM_ [0..n-1] $ \i -> writeArray parent i i
  compRef <- newSTRef n
  let edges = [ (distSq (pts!!i) (pts!!j), i, j)
              | i <- [0..n-2], j <- [i+1..n-1] ]
  let sorted = sortBy (compare `on` (\(d,_,_) -> d)) edges
  let go [] = return Nothing
      go ((_,u,v):es) = do
        merged <- union parent rank u v
        when merged $ modifySTRef' compRef (subtract 1)
        comps <- readSTRef compRef
        if comps == 1 then return $ Just (pts!!u, pts!!v) else go es
  go sorted

parseLine :: String -> Maybe Point
parseLine s = case map reads $ split ',' s of
  [[(x,"")],[(y,"")],[(z,"")]] -> Just (x,y,z)
  _ -> Nothing
  where
    split _ [] = []
    split c xs = let (h,t) = break (==c) xs
                 in h : case t of [] -> [] ; (_:rest) -> split c rest

main :: IO ()
main = do
  exists <- doesFileExist "input.txt"
  when exists $ do
    content <- readFile "input.txt"
    let pts = mapMaybe parseLine $ lines content
    case connectAll pts of
      Just (p1@(x1,_,_), p2@(x2,_,_)) -> do
        putStrLn $ "Connected " ++ showPoint p1 ++ " and " ++ showPoint p2
        putStrLn $ "Product of X coordinates: " ++ show (x1 * x2)
      Nothing -> return ()
  where
    doesFileExist = fmap not . fmap null . readFile
    showPoint (x,y,z) = show x ++ "," ++ show y ++ "," ++ show z
    mapMaybe _ [] = []
    mapMaybe f (x:xs) = case f x of
                         Just y  -> y : mapMaybe f xs
                         Nothing -> mapMaybe f xs
