
import strutils, tables, deques

type Comp = ref object
  mem: Table[int64, int64]
  ip, rb: int64
  inputs: Deque[int64]
  outputs: seq[int64]
  idle: bool

proc getAddr(c: Comp, idx: int64, modes: int64): int64 =
  let m = (if idx == 1: modes mod 10 
           elif idx == 2: (modes div 10) mod 10 
           else: (modes div 100) mod 10)
  let v = c.mem.getOrDefault(c.ip + idx, 0)
  if m == 0: v elif m == 1: c.ip + idx else: c.rb + v

proc run(c: Comp) =
  while true:
    let instr = c.mem.getOrDefault(c.ip, 0)
    let op = instr mod 100
    let modes = instr div 100
    case op:
      of 1:
        c.mem[c.getAddr(3, modes)] = c.mem.getOrDefault(c.getAddr(1, modes), 0) + c.mem.getOrDefault(c.getAddr(2, modes), 0)
        c.ip += 4
      of 2:
        c.mem[c.getAddr(3, modes)] = c.mem.getOrDefault(c.getAddr(1, modes), 0) * c.mem.getOrDefault(c.getAddr(2, modes), 0)
        c.ip += 4
      of 3:
        if c.inputs.len == 0:
          c.mem[c.getAddr(1, modes)] = -1
          c.ip += 2
          c.idle = true
          return
        c.idle = false
        c.mem[c.getAddr(1, modes)] = c.inputs.popFirst()
        c.ip += 2
      of 4:
        c.outputs.add(c.mem.getOrDefault(c.getAddr(1, modes), 0))
        c.ip += 2
        if c.outputs.len == 3: return
      of 5:
        if c.mem.getOrDefault(c.getAddr(1, modes), 0) != 0: c.ip = c.mem.getOrDefault(c.getAddr(2, modes), 0)
        else: c.ip += 3
      of 6:
        if c.mem.getOrDefault(c.getAddr(1, modes), 0) == 0: c.ip = c.mem.getOrDefault(c.getAddr(2, modes), 0)
        else: c.ip += 3
      of 7:
        c.mem[c.getAddr(3, modes)] = if c.mem.getOrDefault(c.getAddr(1, modes), 0) < c.mem.getOrDefault(c.getAddr(2, modes), 0): 1 else: 0
        c.ip += 4
      of 8:
        c.mem[c.getAddr(3, modes)] = if c.mem.getOrDefault(c.getAddr(1, modes), 0) == c.mem.getOrDefault(c.getAddr(2, modes), 0): 1 else: 0
        c.ip += 4
      of 9:
        c.rb += c.mem.getOrDefault(c.getAddr(1, modes), 0)
        c.ip += 2
      of 99: return
      else: discard

let raw = readFile("input.txt").strip().split(',')
var program = initTable[int64, int64]()
for i, v in raw: program[i.int64] = v.parseBiggestInt()

var comps: array[50, Comp]
for i in 0..49:
  comps[i] = Comp(mem: program, ip: 0, rb: 0, inputs: initDeque[int64](), outputs: @[], idle: false)
  comps[i].inputs.addLast(i.int64)

var nx, ny, ly: int64
var ne, hly = false
while true:
  var active = false
  for i in 0..49:
    comps[i].run()
    while comps[i].outputs.len >= 3:
      let d = comps[i].outputs[0]
      let x = comps[i].outputs[1]
      let y = comps[i].outputs[2]
      comps[i].outputs = @[]
      if d == 255: (nx, ny, ne) = (x, y, true)
      elif d >= 0 and d < 50:
        comps[d].inputs.addLast(x)
        comps[d].inputs.addLast(y)
    if comps[i].inputs.len > 0 or not comps[i].idle: active = true
  if not active and ne:
    if hly and ny == ly:
      echo ny
      quit()
    comps[0].inputs.addLast(nx)
    comps[0].inputs.addLast(ny)
    ly = ny
    hly = true
    for i in 0..49: comps[i].idle = false

