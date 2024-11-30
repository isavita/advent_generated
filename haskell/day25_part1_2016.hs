
import Data.List (elemIndex)
import Data.Maybe (fromJust)
import qualified Data.Map.Strict as Map

type Registers = Map.Map String Int
type Instruction = String

main :: IO ()
main = do
    instructions <- lines <$> readFile "input.txt"
    print $ head [a | a <- [1..], producesClockSignal a instructions]

producesClockSignal :: Int -> [Instruction] -> Bool
producesClockSignal a instructions = go a (Map.fromList [("a", a), ("b", 0), ("c", 0), ("d", 0)]) 0 0 0
  where
    go a regs i lastOut outCount
      | outCount > 50 = True
      | i >= length instructions = False
      | otherwise = 
        let inst = instructions !! i
            parts = words inst
        in case head parts of
            "cpy" -> go a (Map.insert (last parts) (getValue (parts !! 1) regs) regs) (i + 1) lastOut outCount
            "inc" -> go a (Map.adjust (+1) (last parts) regs) (i + 1) lastOut outCount
            "dec" -> go a (Map.adjust (subtract 1) (last parts) regs) (i + 1) lastOut outCount
            "jnz" -> 
                let val = getValue (parts !! 1) regs
                    jump = read (parts !! 2)
                in go a regs (if val /= 0 then i + jump else i + 1) lastOut outCount
            "out" ->
                let val = getValue (parts !! 1) regs
                in if val `notElem` [0, 1] || (outCount > 0 && val == lastOut)
                   then False
                   else go a regs (i + 1) val (outCount + 1)

getValue :: String -> Registers -> Int
getValue s regs = case reads s of
    [(n, "")] -> n
    _ -> regs Map.! s
