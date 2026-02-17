
import strutils, tables, deques, sequtils

type
  Packet = tuple[x, y: int64]
  Computer = ref object
    mem: Table[int64, int64]
    ip, rb: int64
    input: Deque[int64]
    output: seq[int64]

proc getVal(c: Computer, mode: int, offset: int64): int64 =
  let v = c.mem.getOrDefault(c.ip + offset)
  case mode
  of 0: c.mem.getOrDefault(v)
  of 1: v
  of 2: c.mem.getOrDefault(c.rb + v)
  else: 0

proc getAddr(c: Computer, mode: int, offset: int64): int64 =
  let v = c.mem.getOrDefault(c.ip + offset)
  if mode == 2: c.rb + v else: v

proc run(c: Computer) =
  while true:
    let instr = c.mem.getOrDefault(c.ip)
    let op = instr mod 100
    let m1 = (instr div 100) mod 10
    let m2 = (instr div 1000) mod 10
    let m3 = (instr div 10000) mod 10
    case op
    of 1:
      c.mem[c.getAddr(m3.int, 3)] = c.getVal(m1.int, 1) + c.getVal(m2.int, 2)
      c.ip += 4
    of 2:
      c.mem[c.getAddr(m3.int, 3)] = c.getVal(m1.int, 1) * c.getVal(m2.int, 2)
      c.ip += 4
    of 3:
      if c.input.len == 0: return
      c.mem[c.getAddr(m1.int, 1)] = c.input.popFirst()
      c.ip += 2
    of 4:
      c.output.add(c.getVal(m1.int, 1))
      c.ip += 2
      if c.output.len == 3: return
    of 5:
      if c.getVal(m1.int, 1) != 0: c.ip = c.getVal(m2.int, 2) else: c.ip += 3
    of 6:
      if c.getVal(m1.int, 1) == 0: c.ip = c.getVal(m2.int, 2) else: c.ip += 3
    of 7:
      c.mem[c.getAddr(m3.int, 3)] = if c.getVal(m1.int, 1) < c.getVal(m2.int, 2): 1 else: 0
      c.ip += 4
    of 8:
      c.mem[c.getAddr(m3.int, 3)] = if c.getVal(m1.int, 1) == c.getVal(m2.int, 2): 1 else: 0
      c.ip += 4
    of 9:
      c.rb += c.getVal(m1.int, 1)
      c.ip += 2
    of 99: return
    else: return

let program = readFile("input.txt").strip().split(',').map(parseBiggestInt)
var comps = newSeq[Computer](50)
var qs = newSeq[Deque[Packet]](50)

for i in 0..49:
  comps[i] = Computer(mem: initTable[int64, int64](), input: initDeque[int64]())
  for idx, v in program: comps[i].mem[idx.int64] = v
  comps[i].input.addLast(i.int64)
  qs[i] = initDeque[Packet]()

while true:
  for i in 0..49:
    if qs[i].len > 0:
      let p = qs[i].popFirst()
      comps[i].input.addLast(p.x)
      comps[i].input.addLast(p.y)
    else:
      comps[i].input.addLast(-1)
    
    comps[i].run()
    
    if comps[i].output.len == 3:
      let dest = comps[i].output[0]
      let x = comps[i].output[1]
      let y = comps[i].output[2]
      comps[i].output = @[]
      if dest == 255:
        echo y
        quit()
      if dest >= 0 and dest < 50:
        qs[dest.int].addLast((x, y))

