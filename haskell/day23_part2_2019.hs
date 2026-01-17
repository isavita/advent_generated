
import qualified Data.IntMap.Strict as M
import Data.Int (Int64)
import Data.Maybe (fromMaybe)

type Mem = M.IntMap Int64

data Computer = Computer
    { pc   :: Int
    , rb   :: Int
    , mem  :: Mem
    , ins  :: [Int64]
    , outs :: [Int64]
    }

val :: Mem -> Int -> Int64
val m addr = fromMaybe 0 (M.lookup addr m)

step :: Computer -> (Computer, Bool)
step c@(Computer p r m i o) =
    let instr = val m p
        op = instr `mod` 100
        mode n = (instr `div` (10 ^ (n + 1))) `mod` 10
        get n = case mode n of
            0 -> val m (fromIntegral (val m (p + n)))
            1 -> val m (p + n)
            2 -> val m (fromIntegral (val m (p + n)) + r)
            _ -> 0
        set n v = case mode n of
            0 -> M.insert (fromIntegral (val m (p + n))) v m
            2 -> M.insert (fromIntegral (val m (p + n)) + r) v m
            _ -> m
    in case op of
        1 -> step $ Computer (p+4) r (set 3 (get 1 + get 2)) i o
        2 -> step $ Computer (p+4) r (set 3 (get 1 * get 2)) i o
        3 -> case i of
            [] -> (c, True)
            (x:xs) -> step $ Computer (p+2) r (set 1 x) xs o
        4 -> step $ Computer (p+2) r m i (o ++ [get 1])
        5 -> step $ Computer (if get 1 /= 0 then fromIntegral (get 2) else p+3) r m i o
        6 -> step $ Computer (if get 1 == 0 then fromIntegral (get 2) else p+3) r m i o
        7 -> step $ Computer (p+4) r (set 3 (if get 1 < get 2 then 1 else 0)) i o
        8 -> step $ Computer (p+4) r (set 3 (if get 1 == get 2 then 1 else 0)) i o
        9 -> step $ Computer (p+2) (r + fromIntegral (get 1)) m i o
        99 -> (c, True)
        _ -> (c, True)

chunk3 :: [Int64] -> [(Int64, Int64, Int64)]
chunk3 (a:b:c:xs) = (a, b, c) : chunk3 xs
chunk3 _ = []

route :: [(Int64, Int64, Int64)] -> M.IntMap Computer -> Maybe (Int64, Int64) -> (M.IntMap Computer, Maybe (Int64, Int64))
route [] m n = (m, n)
route ((d,x,y):ps) m n
    | d == 255 = route ps m (Just (x, y))
    | d >= 0 && d < 50 = route ps (M.adjust (\c -> c { ins = ins c ++ [x, y] }) (fromIntegral d) m) n
    | otherwise = route ps m n

network :: M.IntMap Computer -> Maybe (Int64, Int64) -> Maybe Int64 -> Int64
network nodes nat lastY =
    let stepped = M.map step nodes
        newNodes = M.map fst stepped
        allOuts = concat [ chunk3 (outs (newNodes M.! k)) | k <- [0..49] ]
        cleared = M.map (\c -> c { outs = [] }) newNodes
        (routed, nextNat) = route allOuts cleared nat
        systemIdle = null allOuts && all (\k -> null (ins (routed M.! k))) [0..49]
        fedNodes = M.map (\c -> if null (ins c) then c { ins = [-1] } else c) routed
    in if systemIdle
       then case nextNat of
              Just (nx, ny) -> if Just ny == lastY then ny
                               else network (M.adjust (\c -> c { ins = ins c ++ [nx, ny] }) 0 routed) Nothing (Just ny)
              Nothing -> network fedNodes nextNat lastY
       else network fedNodes nextNat lastY

splitOn :: Char -> String -> [String]
splitOn sep s = case break (== sep) s of
    (a, []) -> [a]
    (a, _:b) -> a : splitOn sep b

main :: IO ()
main = do
    content <- readFile "input.txt"
    let program = map read $ splitOn ',' (head $ lines content)
    let memMap = M.fromList $ zip [0..] program
    let nodes = M.fromList [ (i, Computer 0 0 memMap [fromIntegral i] []) | i <- [0..49] ]
    print $ network nodes Nothing Nothing

