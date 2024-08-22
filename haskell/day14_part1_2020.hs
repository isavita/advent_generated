import qualified Data.Map as Map
import Data.Bits
import Data.List
import System.IO

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let instructions = lines contents
        memory = processInstructions instructions "" Map.empty
        result = sum (Map.elems memory)
    print result

processInstructions :: [String] -> String -> Map.Map Int Int -> Map.Map Int Int
processInstructions [] _ memory = memory
processInstructions (instr:instrs) currentMask memory
    | "mask" `isPrefixOf` instr = processInstructions instrs (drop 7 instr) memory
    | otherwise = let (address, value) = parseMemInstruction instr
                      maskedValue = applyMask currentMask value
                  in processInstructions instrs currentMask (Map.insert address maskedValue memory)

parseMemInstruction :: String -> (Int, Int)
parseMemInstruction instr = let address = read (takeWhile (/= ']') (drop 4 instr)) :: Int
                                value = read (drop 2 (dropWhile (/= '=') instr)) :: Int
                            in (address, value)

applyMask :: String -> Int -> Int
applyMask mask value = foldl' applyBit value (zip [35,34..0] mask)
    where applyBit v (i, 'X') = v
          applyBit v (i, '1') = v .|. (1 `shiftL` i)
          applyBit v (i, '0') = v .&. complement (1 `shiftL` i)