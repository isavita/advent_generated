
import strutils, sequtils, tables

proc getParam(memory: Table[int, int], modes: string, ip: int, offset: int, relativeBase: int): int =
  var mode = 0
  if modes.len >= offset:
    mode = modes[modes.len - offset].int - 48
  let param = memory.getOrDefault(ip + offset, 0)
  case mode
  of 0: return memory.getOrDefault(param, 0)
  of 1: return param
  of 2: return memory.getOrDefault(relativeBase + param, 0)
  else: raise newException(ValueError, "unknown parameter mode")

proc setParam(memory: var Table[int, int], modes: string, ip: int, offset: int, relativeBase: int, value: int) =
  var mode = 0
  if modes.len >= offset:
    mode = modes[modes.len - offset].int - 48
  let param = memory.getOrDefault(ip + offset, 0)
  case mode
  of 0: memory[param] = value
  of 2: memory[relativeBase + param] = value
  else: raise newException(ValueError, "unknown parameter mode")

proc runIntcode(memory: var Table[int, int]): int =
  var output = 0
  var ip = 0
  var relativeBase = 0
  while true:
    let opcode = memory.getOrDefault(ip, 0) mod 100
    let modes = $(memory.getOrDefault(ip, 0) div 100)
    case opcode
    of 1:
      setParam(memory, modes, ip, 3, relativeBase, getParam(memory, modes, ip, 1, relativeBase) + getParam(memory, modes, ip, 2, relativeBase))
      ip += 4
    of 2:
      setParam(memory, modes, ip, 3, relativeBase, getParam(memory, modes, ip, 1, relativeBase) * getParam(memory, modes, ip, 2, relativeBase))
      ip += 4
    of 3:
      setParam(memory, modes, ip, 1, relativeBase, 1)
      ip += 2
    of 4:
      output = getParam(memory, modes, ip, 1, relativeBase)
      ip += 2
    of 5:
      if getParam(memory, modes, ip, 1, relativeBase) != 0:
        ip = getParam(memory, modes, ip, 2, relativeBase)
      else:
        ip += 3
    of 6:
      if getParam(memory, modes, ip, 1, relativeBase) == 0:
        ip = getParam(memory, modes, ip, 2, relativeBase)
      else:
        ip += 3
    of 7:
      if getParam(memory, modes, ip, 1, relativeBase) < getParam(memory, modes, ip, 2, relativeBase):
        setParam(memory, modes, ip, 3, relativeBase, 1)
      else:
        setParam(memory, modes, ip, 3, relativeBase, 0)
      ip += 4
    of 8:
      if getParam(memory, modes, ip, 1, relativeBase) == getParam(memory, modes, ip, 2, relativeBase):
        setParam(memory, modes, ip, 3, relativeBase, 1)
      else:
        setParam(memory, modes, ip, 3, relativeBase, 0)
      ip += 4
    of 9:
      relativeBase += getParam(memory, modes, ip, 1, relativeBase)
      ip += 2
    of 99:
      return output
    else:
      raise newException(ValueError, "unknown opcode: " & $opcode)

var memory = initTable[int, int]()
var program = readFile("input.txt").strip().split(",")
for i, s in program:
  memory[i] = parseInt(s)
echo runIntcode(memory)
