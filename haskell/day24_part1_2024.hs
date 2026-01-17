
import qualified Data.Map as M
import Data.List (isPrefixOf, sort)
import Data.Bits ((.&.), (.|.), xor)

main :: IO ()
main = do
    content <- readFile "input.txt"
    let ls = lines content
        (wLines, gLines) = break null ls
        initial = M.fromList [(init k, read v :: Int) | l <- wLines, let [k, v] = words l]
        gates = M.fromList [(out, (i1, op, i2)) | l <- drop 1 gLines, let [i1, op, i2, _, out] = words l]
        evalMap = M.fromList [(k, eval k) | k <- M.keys initial ++ M.keys gates]
        eval k = case M.lookup k initial of
            Just v -> v
            Nothing -> let (i1, op, i2) = gates M.! k
                           v1 = evalMap M.! i1
                           v2 = evalMap M.! i2
                       in case op of
                            "AND" -> v1 .&. v2
                            "OR"  -> v1 .|. v2
                            "XOR" -> v1 `xor` v2
        zKeys = reverse . sort . filter ("z" `isPrefixOf`) $ M.keys evalMap
        ans = foldl (\acc k -> acc * 2 + fromIntegral (evalMap M.! k)) (0 :: Integer) zKeys
    print ans

