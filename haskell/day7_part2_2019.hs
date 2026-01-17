
import qualified Data.IntMap.Strict as M
import Data.List (permutations)

run mem ip inputs =
  let instr = mem M.! ip
      get n = let v = mem M.! (ip+n)
                  m = (instr `div` (10^(n+1))) `mod` 10
              in if m == 0 then M.findWithDefault 0 v mem else v
      put n val = M.insert (mem M.! (ip+n)) val mem
  in case instr `mod` 100 of
    1 -> run (put 3 (get 1 + get 2)) (ip+4) inputs
    2 -> run (put 3 (get 1 * get 2)) (ip+4) inputs
    3 -> case inputs of
           (i:is) -> run (put 1 i) (ip+2) is
           [] -> []
    4 -> get 1 : run mem (ip+2) inputs
    5 -> run mem (if get 1 /= 0 then get 2 else ip+3) inputs
    6 -> run mem (if get 1 == 0 then get 2 else ip+3) inputs
    7 -> run (put 3 (if get 1 < get 2 then 1 else 0)) (ip+4) inputs
    8 -> run (put 3 (if get 1 == get 2 then 1 else 0)) (ip+4) inputs
    99 -> []
    _  -> []

main = do
  s <- readFile "input.txt"
  let code = map read . words . map (\c -> if c == ',' then ' ' else c) $ s
      mem = M.fromList (zip [0..] code)
      solve [p1,p2,p3,p4,p5] =
        let out1 = run mem 0 (p1:0:out5)
            out2 = run mem 0 (p2:out1)
            out3 = run mem 0 (p3:out2)
            out4 = run mem 0 (p4:out3)
            out5 = run mem 0 (p5:out4)
        in last out5
  print $ maximum [solve p | p <- permutations [5..9]]

