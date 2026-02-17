
import std/[strutils, sequtils, algorithm]

proc run(program: seq[int], inputs: seq[int]): int =
  var 
    code = program
    ip = 0
    inIdx = 0
    lastOut = 0
    
  while ip < code.len:
    let cmd = code[ip]
    let op = cmd mod 100
    let m1 = (cmd div 100) mod 10
    let m2 = (cmd div 1000) mod 10
    
    template g(idx, mode: int): int =
      if mode == 1: code[ip + idx] else: code[code[ip + idx]]

    case op:
    of 1:
      code[code[ip+3]] = g(1, m1) + g(2, m2)
      ip += 4
    of 2:
      code[code[ip+3]] = g(1, m1) * g(2, m2)
      ip += 4
    of 3:
      code[code[ip+1]] = inputs[inIdx]
      inIdx += 1
      ip += 2
    of 4:
      lastOut = g(1, m1)
      ip += 2
    of 5:
      if g(1, m1) != 0: ip = g(2, m2) else: ip += 3
    of 6:
      if g(1, m1) == 0: ip = g(2, m2) else: ip += 3
    of 7:
      code[code[ip+3]] = if g(1, m1) < g(2, m2): 1 else: 0
      ip += 4
    of 8:
      code[code[ip+3]] = if g(1, m1) == g(2, m2): 1 else: 0
      ip += 4
    of 99: break
    else: break
  return lastOut

let initialCode = readFile("input.txt").strip().split(',').map(parseInt)
var 
  phases = @[0, 1, 2, 3, 4]
  maxOut = 0

while true:
  var signal = 0
  for p in phases:
    signal = run(initialCode, @[p, signal])
  if signal > maxOut: maxOut = signal
  if not phases.nextPermutation(): break

echo maxOut
