import std/[sequtils, strutils, os]

proc loadProgram(): seq[int] =
  let data = readFile("input.txt")
  data.split(',').mapIt(parseInt(it))

proc runVM(prog: seq[int]; inp: array[2, int]): int =
  var mem = newSeq[int](10000)
  for i, v in prog: mem[i] = v
  var ip = 0
  var rel = 0
  var input = inp
  while true:
    let instr = mem[ip]
    let op = instr mod 100
    let mode = [(instr div 100) mod 10, (instr div 1000) mod 10, (instr div 10000) mod 10]
    proc getVal(p: int, m: int): int =
      case m
      of 0: mem[mem[p]]
      of 1: mem[p]
      else: mem[rel + mem[p]]
    proc getAddr(p: int, m: int): int =
      case m
      of 0: mem[p]
      of 2: rel + mem[p]
      else: p
    case op
    of 1:
      let a = getVal(ip+1, mode[0])
      let b = getVal(ip+2, mode[1])
      let c = getAddr(ip+3, mode[2])
      mem[c] = a + b
      ip += 4
    of 2:
      let a = getVal(ip+1, mode[0])
      let b = getVal(ip+2, mode[1])
      let c = getAddr(ip+3, mode[2])
      mem[c] = a * b
      ip += 4
    of 3:
      let a = getAddr(ip+1, mode[0])
      mem[a] = input[0]
      input[0] = input[1]
      ip += 2
    of 4:
      return getVal(ip+1, mode[0])
    of 5:
      let a = getVal(ip+1, mode[0])
      let b = getVal(ip+2, mode[1])
      ip = if a != 0: b else: ip + 3
    of 6:
      let a = getVal(ip+1, mode[0])
      let b = getVal(ip+2, mode[1])
      ip = if a == 0: b else: ip + 3
    of 7:
      let a = getVal(ip+1, mode[0])
      let b = getVal(ip+2, mode[1])
      let c = getAddr(ip+3, mode[2])
      mem[c] = if a < b: 1 else: 0
      ip += 4
    of 8:
      let a = getVal(ip+1, mode[0])
      let b = getVal(ip+2, mode[1])
      let c = getAddr(ip+3, mode[2])
      mem[c] = if a == b: 1 else: 0
      ip += 4
    of 9:
      rel += getVal(ip+1, mode[0])
      ip += 2
    of 99:
      break
    else:
      break
  0

proc beam(x, y: int; prog: seq[int]): bool =
  runVM(prog, [x, y]) == 1

let program = loadProgram()
var count = 0
for y in 0..<50:
  for x in 0..<50:
    if beam(x, y, program):
      inc count
echo count