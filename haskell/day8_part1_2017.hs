
import qualified Data.Map as Map

main = do
    contents <- readFile "input.txt"
    let instructions = map words $ lines contents
        registers = Map.empty
        updatedRegisters = foldl processInstruction registers instructions
        maxValue = maximum $ Map.elems updatedRegisters
    print maxValue

processInstruction registers [reg, op, amountStr, _, condReg, condOp, condValStr] =
    if checkCondition registers condReg condOp (read condValStr)
        then updateRegister registers reg op (read amountStr)
        else registers

checkCondition registers condReg condOp condVal =
    case condOp of
        ">"  -> Map.findWithDefault 0 condReg registers > condVal
        ">=" -> Map.findWithDefault 0 condReg registers >= condVal
        "<"  -> Map.findWithDefault 0 condReg registers < condVal
        "<=" -> Map.findWithDefault 0 condReg registers <= condVal
        "==" -> Map.findWithDefault 0 condReg registers == condVal
        "!=" -> Map.findWithDefault 0 condReg registers /= condVal

updateRegister registers reg op amount =
    case op of
        "inc" -> Map.insertWith (+) reg amount registers
        "dec" -> Map.insertWith (+) reg (-amount) registers
