import os
import strutils
import sequtils
import tables

type
  Intcode = object
    memory: seq[int]
    ip: int
    relativeBase: int

proc newIntcode(program: seq[int]): Intcode =
  result.memory = program
  result.memory.setLen(10000)
  result.ip = 0
  result.relativeBase = 0

proc getParam(intcode: var Intcode, index: int, mode: int, isWrite: bool = false): int =
  case mode
  of 0:
    if isWrite:
      return intcode.memory[index]
    else:
      return intcode.memory[intcode.memory[index]]
  of 1:
    return intcode.memory[index]
  of 2:
    if isWrite:
      return intcode.memory[index] + intcode.relativeBase
    else:
      return intcode.memory[intcode.memory[index] + intcode.relativeBase]
  else:
    raise newException(IOError, "Unknown parameter mode")

proc runIntcode(intcode: var Intcode, input: int): int =
  while true:
    let opcode = intcode.memory[intcode.ip] mod 100
    let modes = intcode.memory[intcode.ip] div 100
    case opcode
    of 1:
      let a = intcode.getParam(intcode.ip + 1, modes mod 10)
      let b = intcode.getParam(intcode.ip + 2, (modes div 10) mod 10)
      let c = intcode.getParam(intcode.ip + 3, (modes div 100) mod 10, true)
      intcode.memory[c] = a + b
      intcode.ip += 4
    of 2:
      let a = intcode.getParam(intcode.ip + 1, modes mod 10)
      let b = intcode.getParam(intcode.ip + 2, (modes div 10) mod 10)
      let c = intcode.getParam(intcode.ip + 3, (modes div 100) mod 10, true)
      intcode.memory[c] = a * b
      intcode.ip += 4
    of 3:
      let a = intcode.getParam(intcode.ip + 1, modes mod 10, true)
      intcode.memory[a] = input
      intcode.ip += 2
    of 4:
      let a = intcode.getParam(intcode.ip + 1, modes mod 10)
      intcode.ip += 2
      return a
    of 5:
      let a = intcode.getParam(intcode.ip + 1, modes mod 10)
      let b = intcode.getParam(intcode.ip + 2, (modes div 10) mod 10)
      if a != 0:
        intcode.ip = b
      else:
        intcode.ip += 3
    of 6:
      let a = intcode.getParam(intcode.ip + 1, modes mod 10)
      let b = intcode.getParam(intcode.ip + 2, (modes div 10) mod 10)
      if a == 0:
        intcode.ip = b
      else:
        intcode.ip += 3
    of 7:
      let a = intcode.getParam(intcode.ip + 1, modes mod 10)
      let b = intcode.getParam(intcode.ip + 2, (modes div 10) mod 10)
      let c = intcode.getParam(intcode.ip + 3, (modes div 100) mod 10, true)
      if a < b:
        intcode.memory[c] = 1
      else:
        intcode.memory[c] = 0
      intcode.ip += 4
    of 8:
      let a = intcode.getParam(intcode.ip + 1, modes mod 10)
      let b = intcode.getParam(intcode.ip + 2, (modes div 10) mod 10)
      let c = intcode.getParam(intcode.ip + 3, (modes div 100) mod 10, true)
      if a == b:
        intcode.memory[c] = 1
      else:
        intcode.memory[c] = 0
      intcode.ip += 4
    of 9:
      let a = intcode.getParam(intcode.ip + 1, modes mod 10)
      intcode.relativeBase += a
      intcode.ip += 2
    of 99:
      return -1
    else:
      raise newException(IOError, "Unknown opcode")

proc bfs(intcode: Intcode): int =
  var queue: seq[(int, int, int, Intcode)] = @[(0, 0, 0, intcode)]
  var visited: Table[(int, int), bool] = initTable[(int, int), bool]()
  visited[(0, 0)] = true
  let directions = @[(0, 1), (0, -1), (-1, 0), (1, 0)]
  let commands = @[1, 2, 3, 4]
  while queue.len > 0:
    let (x, y, steps, intcode) = queue[0]
    queue.delete(0)
    for i, dir in directions:
      let newX = x + dir[0]
      let newY = y + dir[1]
      if visited.hasKey((newX, newY)):
        continue
      var newIntcode = intcode
      let status = newIntcode.runIntcode(commands[i])
      if status == 0:
        visited[(newX, newY)] = true
        continue
      if status == 2:
        return steps + 1
      visited[(newX, newY)] = true
      queue.add((newX, newY, steps + 1, newIntcode))
  return -1

when isMainModule:
  let input = readFile("input.txt").strip().split(',')
  var program: seq[int] = @[]
  for num in input:
    program.add(parseInt(num))
  let intcode = newIntcode(program)
  echo bfs(intcode)