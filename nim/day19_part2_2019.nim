
import tables, strutils

proc run(m: Table[int64, int64], x, y: int64): int =
  var mem = initTable[int64, int64]()
  for k, v in m.pairs: mem[k] = v
  var ip, rb = 0i64
  let inputs = [x, y]
  var inPtr = 0
  while true:
    let cmd = mem.getOrDefault(ip, 0)
    let op = cmd mod 100
    template arg(i: int64): int64 =
      let mode = (cmd div (if i == 0: 100i64 elif i == 1: 1000i64 else: 10000i64)) mod 10
      let v0 = mem.getOrDefault(ip + i + 1, 0)
      if mode == 0: v0 elif mode == 1: ip + i + 1 else: rb + v0
    template v(i: int64): int64 = mem.getOrDefault(arg(i), 0)
    case op:
      of 1: mem[arg(2)] = v(0) + v(1); ip += 4
      of 2: mem[arg(2)] = v(0) * v(1); ip += 4
      of 3: mem[arg(0)] = inputs[inPtr]; inPtr += 1; ip += 2
      of 4: return v(0).int
      of 5: 
        if v(0) != 0: ip = v(1)
        else: ip += 3
      of 6: 
        if v(0) == 0: ip = v(1)
        else: ip += 3
      of 7: mem[arg(2)] = (if v(0) < v(1): 1i64 else: 0i64); ip += 4
      of 8: mem[arg(2)] = (if v(0) == v(1): 1i64 else: 0i64); ip += 4
      of 9: rb += v(0); ip += 2
      of 99: return 0
      else: return 0

let inputData = readFile("input.txt").strip().split(',')
var initialMem = initTable[int64, int64]()
for i, s in inputData:
  if s != "": initialMem[i.int64] = s.parseBiggestInt

var x = 0i64
var y = 20i64
while true:
  if run(initialMem, x, y) == 0: x += 1
  elif run(initialMem, x + 99, y) == 0: y += 1
  elif run(initialMem, x, y + 99) == 0: x += 1
  else:
    echo x * 10000 + y
    break

