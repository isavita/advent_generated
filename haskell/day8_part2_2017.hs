
import qualified Data.Map as Map
import System.IO

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let instructions = map words (lines contents)
        (registers, highestValue) = foldl processInstruction (Map.empty, 0) instructions
    print highestValue

processInstruction :: (Map.Map String Int, Int) -> [String] -> (Map.Map String Int, Int)
processInstruction (regs, highest) [reg, op, amtStr, _, condReg, condOp, condValStr] =
    let amount = read amtStr
        condVal = read condValStr
        cond = case condOp of
            ">"  -> Map.findWithDefault 0 condReg regs > condVal
            ">=" -> Map.findWithDefault 0 condReg regs >= condVal
            "<"  -> Map.findWithDefault 0 condReg regs < condVal
            "<=" -> Map.findWithDefault 0 condReg regs <= condVal
            "==" -> Map.findWithDefault 0 condReg regs == condVal
            "!=" -> Map.findWithDefault 0 condReg regs /= condVal
    in if cond
       then let newRegValue = case op of
                    "inc" -> Map.findWithDefault 0 reg regs + amount
                    "dec" -> Map.findWithDefault 0 reg regs - amount
            in (Map.insert reg newRegValue regs, max highest newRegValue)
       else (regs, highest)
