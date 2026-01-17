
import qualified Data.Map.Strict as M

data Computer = Computer { mem :: M.Map Int Integer, ip :: Int, rb :: Int, halted :: Bool }

step :: [Integer] -> Computer -> ([Integer], Computer)
step inputs c@(Computer m ip rb h)
  | h = ([], c)
  | otherwise = case op `mod` 100 of
      1 -> step inputs c { mem = M.insert (addr 3) (val 1 + val 2) m, ip = ip + 4 }
      2 -> step inputs c { mem = M.insert (addr 3) (val 1 * val 2) m, ip = ip + 4 }
      3 -> case inputs of
             [] -> ([], c)
             (x:xs) -> step xs c { mem = M.insert (addr 1) x m, ip = ip + 2 }
      4 -> let (os, nc) = step inputs c { ip = ip + 2 } in (val 1 : os, nc)
      5 -> step inputs c { ip = if val 1 /= 0 then fromIntegral (val 2) else ip + 3 }
      6 -> step inputs c { ip = if val 1 == 0 then fromIntegral (val 2) else ip + 3 }
      7 -> step inputs c { mem = M.insert (addr 3) (if val 1 < val 2 then 1 else 0) m, ip = ip + 4 }
      8 -> step inputs c { mem = M.insert (addr 3) (if val 1 == val 2 then 1 else 0) m, ip = ip + 4 }
      9 -> step inputs c { rb = rb + fromIntegral (val 1), ip = ip + 2 }
      99 -> ([], c { halted = True })
  where
    op = fromIntegral (M.findWithDefault 0 ip m)
    mode i = (op `div` (10 ^ (i + 1))) `mod` 10
    val i = case mode i of
      0 -> M.findWithDefault 0 (fromIntegral $ M.findWithDefault 0 (ip + i) m) m
      1 -> M.findWithDefault 0 (ip + i) m
      _ -> M.findWithDefault 0 (fromIntegral $ M.findWithDefault 0 (ip + i) m + fromIntegral rb) m
    addr i = case mode i of
      0 -> fromIntegral $ M.findWithDefault 0 (ip + i) m
      _ -> fromIntegral $ M.findWithDefault 0 (ip + i) m + fromIntegral rb

runRobot hull pos dir comp
  | halted comp = hull
  | otherwise =
      let (outs, nextComp) = step [M.findWithDefault 0 pos hull] comp
      in process hull pos dir nextComp outs
  where
    process h p d c (color:turn:rest) =
      let nDir = (if turn == 0 then d + 3 else d + 1) `mod` 4
          (dx, dy) = [(0,-1), (1,0), (0,1), (-1,0)] !! nDir
          (x, y) = p
      in process (M.insert p color h) (x+dx, y+dy) nDir c rest
    process h p d c _ = runRobot h p d c

main :: IO ()
main = do
  s <- readFile "input.txt"
  let prog = read $ "[" ++ s ++ "]" :: [Integer]
      comp = Computer (M.fromList $ zip [0..] prog) 0 0 False
  print $ M.size $ runRobot M.empty (0,0) 0 comp
