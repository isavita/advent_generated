
import std/[strutils, tables, strscans]

proc main() =
  var 
    registers = initTable[string, int]()
    highestValue = 0

  for line in lines("input.txt"):
    var 
      reg, op: string
      amount, condVal: int
      condReg, condOp: string

    if scanf(line, "$+ $+ $i if $+ $+ $i", reg, op, amount, condReg, condOp, condVal):
      var condResult = false
      
      case condOp:
        of ">": condResult = registers.getOrDefault(condReg) > condVal
        of ">=": condResult = registers.getOrDefault(condReg) >= condVal
        of "<": condResult = registers.getOrDefault(condReg) < condVal
        of "<=": condResult = registers.getOrDefault(condReg) <= condVal
        of "==": condResult = registers.getOrDefault(condReg) == condVal
        of "!=": condResult = registers.getOrDefault(condReg) != condVal

      if condResult:
        var currentVal = registers.getOrDefault(reg)
        if op == "inc":
          currentVal += amount
        else:
          currentVal -= amount
        
        registers[reg] = currentVal
        highestValue = max(highestValue, currentVal)

  echo highestValue

main()
