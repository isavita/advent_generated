
import std/[strutils, sequtils, tables, deques]

type
  Computer = ref object
    mem: Table[int64, int64]
    pc, rb: int64
    inputs, outputs: Deque[int64]
    halted: bool

proc newComputer(prog: seq[int64]): Computer =
  result = Computer(mem: initTable[int64, int64](), pc: 0, rb: 0, halted: false)
  for i, v in prog: result.mem[i.int64] = v

proc get(c: Computer, mode: int, val: int64): int64 =
  case mode:
    of 0: result = c.mem.getOrDefault(val)
    of 1: result = val
    of 2: result = c.mem.getOrDefault(c.rb + val)
    else: discard

proc set(c: Computer, mode: int, reg: int64, val: int64) =
  case mode:
    of 0: c.mem[reg] = val
    of 2: c.mem[c.rb + reg] = val
    else: discard

proc run(c: Computer) =
  while not c.halted:
    let instr = c.mem[c.pc]
    let op = instr mod 100
    let m = [(instr div 100) mod 10, (instr div 1000) mod 10, (instr div 10000) mod 10]
    case op:
      of 1:
        c.set(m[2].int, c.mem[c.pc+3], c.get(m[0].int, c.mem[c.pc+1]) + c.get(m[1].int, c.mem[c.pc+2]))
        c.pc += 4
      of 2:
        c.set(m[2].int, c.mem[c.pc+3], c.get(m[0].int, c.mem[c.pc+1]) * c.get(m[1].int, c.mem[c.pc+2]))
        c.pc += 4
      of 3:
        if c.inputs.len == 0: return
        c.set(m[0].int, c.mem[c.pc+1], c.inputs.popFirst)
        c.pc += 2
      of 4:
        c.outputs.addLast(c.get(m[0].int, c.mem[c.pc+1]))
        c.pc += 2
      of 5:
        if c.get(m[0].int, c.mem[c.pc+1]) != 0: c.pc = c.get(m[1].int, c.mem[c.pc+2]) else: c.pc += 3
      of 6:
        if c.get(m[0].int, c.mem[c.pc+1]) == 0: c.pc = c.get(m[1].int, c.mem[c.pc+2]) else: c.pc += 3
      of 7:
        c.set(m[2].int, c.mem[c.pc+3], if c.get(m[0].int, c.mem[c.pc+1]) < c.get(m[1].int, c.mem[c.pc+2]): 1 else: 0)
        c.pc += 4
      of 8:
        c.set(m[2].int, c.mem[c.pc+3], if c.get(m[0].int, c.mem[c.pc+1]) == c.get(m[1].int, c.mem[c.pc+2]): 1 else: 0)
        c.pc += 4
      of 9:
        c.rb += c.get(m[0].int, c.mem[c.pc+1])
        c.pc += 2
      of 99: c.halted = true
      else: discard

let prog = readFile("input.txt").strip().split(',').map(parseBiggestInt)

var comp = newComputer(prog)
comp.run()
var grid = newSeq[string]()
var line = ""
while comp.outputs.len > 0:
  let c = char(comp.outputs.popFirst)
  if c == '\L':
    if line != "": grid.add(line)
    line = ""
  else: line.add(c)

let h = grid.len
let w = grid[0].len
var ans1 = 0
for y in 1..<h-1:
  for x in 1..<w-1:
    if grid[y][x] == '#' and grid[y-1][x] == '#' and grid[y+1][x] == '#' and grid[y][x-1] == '#' and grid[y][x+1] == '#':
      ans1 += x * y
echo ans1

var rx, ry: int
var rd: char
for y in 0..<h:
  for x in 0..<w:
    if grid[y][x] in "^v<>": (rx, ry, rd) = (x, y, grid[y][x])

var path = newSeq[string]()
var steps = 0
let dirs = {'^': (0,-1), 'v': (0,1), '<': (-1,0), '>': (1,0)}.toTable
let left = {'^': '<', '<': 'v', 'v': '>', '>': '^'}.toTable
let right = {'^': '>', '>': 'v', 'v': '<', '<': '^'}.toTable

while true:
  let (dx, dy) = dirs[rd]
  let (nx, ny) = (rx + dx, ry + dy)
  if nx >= 0 and nx < w and ny >= 0 and ny < h and grid[ny][nx] == '#':
    rx = nx; ry = ny; steps.inc
  else:
    if steps > 0: path.add($steps); steps = 0
    let ld = left[rd]
    let (lx, ly) = (rx + dirs[ld][0], ry + dirs[ld][1])
    if lx >= 0 and lx < w and ly >= 0 and ly < h and grid[ly][lx] == '#':
      path.add("L"); rd = ld
    else:
      let rr = right[rd]
      let (rx2, ry2) = (rx + dirs[rr][0], ry + dirs[rr][1])
      if rx2 >= 0 and rx2 < w and ry2 >= 0 and ry2 < h and grid[ry2][rx2] == '#':
        path.add("R"); rd = rr
      else: break

proc check(p, a, b, c: seq[string]): string =
  var i = 0
  var res = newSeq[string]()
  while i < p.len:
    if i+a.len <= p.len and p[i..<i+a.len] == a: res.add("A"); i += a.len
    elif i+b.len <= p.len and p[i..<i+b.len] == b: res.add("B"); i += b.len
    elif i+c.len <= p.len and p[i..<i+c.len] == c: res.add("C"); i += c.len
    else: return ""
  let s = res.join(",")
  return if s.len <= 20: s else: ""

var ma, fa, fb, fc = ""
block search:
  for i in 1..10:
    let a = path[0..<i]
    if a.join(",").len > 20: break
    for j in 1..10:
      var k = i
      while k < path.len and path[k..<k+a.len] == a: k += a.len
      if k >= path.len: continue
      for l in 1..10:
        if k+l > path.len: break
        let b = path[k..<k+l]
        if b.join(",").len > 20: break
        for m in 1..10:
          var n = k
          while n < path.len:
            if n+a.len <= path.len and path[n..<n+a.len] == a: n += a.len
            elif n+b.len <= path.len and path[n..<n+b.len] == b: n += b.len
            else: break
          if n >= path.len: continue
          for o in 1..10:
            if n+o > path.len: break
            let c = path[n..<n+o]
            if c.join(",").len > 20: break
            let res = check(path, a, b, c)
            if res != "":
              (ma, fa, fb, fc) = (res, a.join(","), b.join(","), c.join(","))
              break search

var c2 = newComputer(prog)
c2.mem[0] = 2
for s in [ma, fa, fb, fc, "n"]:
  for ch in s: c2.inputs.addLast(ch.int64)
  c2.inputs.addLast(10)
c2.run()
echo c2.outputs.peekLast
