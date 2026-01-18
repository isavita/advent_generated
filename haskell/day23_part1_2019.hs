import qualified Data.IntMap.Strict as M

data Machine = Machine { ip :: Int, rb :: Int, mem :: M.IntMap Int, inputs :: [Int], outputs :: [Int], halted :: Bool }

run m@(Machine p r mm inps outs h)
    | h = m
    | otherwise =
        let get v = M.findWithDefault 0 v mm
            f = get p
            op = f `mod` 100
            mode i = (f `div` (10 ^ (i + 1))) `mod` 10
            val i = case mode i of { 0 -> get (get (p + i)); 1 -> get (p + i); 2 -> get (r + get (p + i)) }
            addr i = case mode i of { 0 -> get (p + i); 2 -> r + get (p + i) }
            set i v = M.insert (addr i) v mm
        in case op of
            1 -> run $ Machine (p + 4) r (set 3 (val 1 + val 2)) inps outs False
            2 -> run $ Machine (p + 4) r (set 3 (val 1 * val 2)) inps outs False
            3 -> case inps of { [] -> m; (x:xs) -> run $ Machine (p + 2) r (set 1 x) xs outs False }
            4 -> run $ Machine (p + 2) r mm inps (outs ++ [val 1]) False
            5 -> run $ Machine (if val 1 /= 0 then val 2 else p + 3) r mm inps outs False
            6 -> run $ Machine (if val 1 == 0 then val 2 else p + 3) r mm inps outs False
            7 -> run $ Machine (p + 4) r (set 3 (if val 1 < val 2 then 1 else 0)) inps outs False
            8 -> run $ Machine (p + 4) r (set 3 (if val 1 == val 2 then 1 else 0)) inps outs False
            9 -> run $ Machine (p + 2) (r + val 1) mm inps outs False
            99 -> m { halted = True }
            _ -> m

repl i v ls = take i ls ++ [v] ++ drop (i + 1) ls
chunk (a:b:c:xs) = (a, b, c) : chunk xs
chunk _ = []

dist [] q = (q, Nothing)
dist ((d, x, y):ps) q
    | d == 255 = (q, Just y)
    | d >= 0 && d < 50 = dist ps (repl d (q !! d ++ [(x, y)]) q)
    | otherwise = dist ps q

stepN [] _ q = ([], q, Nothing)
stepN (m:ms) i q =
    let (cin, nq) = case q !! i of { [] -> ([-1], q); (x, y):rs -> ([x, y], repl i rs q) }
        m' = run m { inputs = inputs m ++ cin }
        pkts = chunk (outputs m')
        (nq', fnd) = dist pkts nq
        m'' = m' { outputs = drop (3 * length pkts) (outputs m') }
    in if fnd /= Nothing then ([], [], fnd)
       else let (rms, rqs, rf) = stepN ms (i + 1) nq' in (m'' : rms, rqs, rf)

solve ms qs = let (ms', qs', res) = stepN ms 0 qs in maybe (solve ms' qs') id res

main = do
    c <- readFile "input.txt"
    let m0 = M.fromList $ zip [0..] $ map read $ words [if x == ',' then ' ' else x | x <- c]
        ms = [ Machine 0 0 m0 [i] [] False | i <- [0..49] ]
    print $ solve ms (replicate 50 [])