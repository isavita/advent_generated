import Data.List
import Data.Maybe
import Data.Char

data Packet = IntPacket Int | ListPacket [Packet] deriving (Eq)

instance Ord Packet where
    compare (IntPacket a) (IntPacket b) = compare a b
    compare (ListPacket a) (ListPacket b) = compare a b
    compare (IntPacket a) (ListPacket b) = compare (ListPacket [IntPacket a]) (ListPacket b)
    compare (ListPacket a) (IntPacket b) = compare (ListPacket a) (ListPacket [IntPacket b])

instance Show Packet where
    show (IntPacket a) = show a
    show (ListPacket a) = "[" ++ intercalate "," (map show a) ++ "]"

parsePacket :: String -> Packet
parsePacket s = fst $ parseList s 0
    where
        parseList :: String -> Int -> (Packet, Int)
        parseList s i
            | s !! i == '[' = let (list, i') = parseElements s (i + 1) in (ListPacket list, i')
            | otherwise = parseInt s i
        parseElements :: String -> Int -> ([Packet], Int)
        parseElements s i
            | s !! i == ']' = ([], i + 1)
            | otherwise = let (elem, i') = parseList s i in
                          let (rest, i'') = parseMoreElements s i' in
                          (elem : rest, i'')
        parseMoreElements :: String -> Int -> ([Packet], Int)
        parseMoreElements s i
            | s !! i == ',' = parseElements s (i + 1)
            | s !! i == ']' = ([], i + 1)
        parseInt :: String -> Int -> (Packet, Int)
        parseInt s i = let (num, i') = span isDigit (drop i s) in (IntPacket (read num), i + length num)

main :: IO ()
main = do
    input <- readFile "input.txt"
    let packets = map parsePacket $ filter (not . null) $ lines input
    let allPackets = packets ++ [ListPacket [ListPacket [IntPacket 2]], ListPacket [ListPacket [IntPacket 6]]]
    let sortedPackets = sort allPackets
    let index1 = fromJust $ elemIndex (ListPacket [ListPacket [IntPacket 2]]) sortedPackets
    let index2 = fromJust $ elemIndex (ListPacket [ListPacket [IntPacket 6]]) sortedPackets
    print $ (index1 + 1) * (index2 + 1)