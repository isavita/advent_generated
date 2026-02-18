
import os, strutils, tables, deques

type Computer = object
  mem: Table[int, int]
  p, relBase: int
  inputs, outputs: Deque[int]
  halted: bool

proc g(c: var Computer, m, v: int): int =
  case m:
    of 0: c.mem.getOrDefault(v, 0)
    of 1: v
    of 2: c.mem.getOrDefault(c.relBase + v, 0)
    else: 0

proc s(c: var Computer, m, v, r: int) =
  let a = if m == 2: c.relBase + v else: v
  c.mem[a] = r

proc run(c: var Computer) =
  while not c.halted:
    let instr = c.mem.getOrDefault(c.p, 0)
    let op = instr mod 100
    let m = [(instr div 100) mod 10, (instr div 1000) mod 10, (instr div 10000) mod 10]
    case op:
      of 1:
        c.s(m[2], c.mem.getOrDefault(c.p+3, 0), c.g(m[0], c.mem.getOrDefault(c.p+1, 0)) + c.g(m[1], c.mem.getOrDefault(c.p+2, 0)))
        c.p += 4
      of 2:
        c.s(m[2], c.mem.getOrDefault(c.p+3, 0), c.g(m[0], c.mem.getOrDefault(c.p+1, 0)) * c.g(m[1], c.mem.getOrDefault(c.p+2, 0)))
        c.p += 4
      of 3:
        if c.inputs.len == 0: return
        c.s(m[0], c.mem.getOrDefault(c.p+1, 0), c.inputs.popFirst)
        c.p += 2
      of 4:
        c.outputs.addLast(c.g(m[0], c.mem.getOrDefault(c.p+1, 0)))
        c.p += 2
        return
      of 5:
        if c.g(m[0], c.mem.getOrDefault(c.p+1, 0)) != 0: c.p = c.g(m[1], c.mem.getOrDefault(c.p+2, 0))
        else: c.p += 3
      of 6:
        if c.g(m[0], c.mem.getOrDefault(c.p+1, 0)) == 0: c.p = c.g(m[1], c.mem.getOrDefault(c.p+2, 0))
        else: c.p += 3
      of 7:
        c.s(m[2], c.mem.getOrDefault(c.p+3, 0), if c.g(m[0], c.mem.getOrDefault(c.p+1, 0)) < c.g(m[1], c.mem.getOrDefault(c.p+2, 0)): 1 else: 0)
        c.p += 4
      of 8:
        c.s(m[2], c.mem.getOrDefault(c.p+3, 0), if c.g(m[0], c.mem.getOrDefault(c.p+1, 0)) == c.g(m[1], c.mem.getOrDefault(c.p+2, 0)): 1 else: 0)
        c.p += 4
      of 9:
        c.relBase += c.g(m[0], c.mem.getOrDefault(c.p+1, 0))
        c.p += 2
      of 99: c.halted = true
      else: return

var comp: Computer
let inputData = readFile("input.txt").strip().split(',')
for i in 0..<inputData.len:
  comp.mem[i] = inputData[i].strip.parseInt

var map = initTable[(int, int), int]()
var visited = initTable[(int, int), bool]()
var oxyPos: (int, int)
let dx = [0, 0, 0, -1, 1]
let dy = [0, -1, 1, 0, 0]
let opp = [0, 2, 1, 4, 3]

proc dfs(x, y: int) =
  for cmd in 1..4:
    let nx = x + dx[cmd]
    let ny = y + dy[cmd]
    if (nx, ny) in visited: continue
    comp.inputs.addLast(cmd)
    comp.run()
    let status = comp.outputs.popFirst()
    map[(nx, ny)] = status
    visited[(nx, ny)] = true
    if status != 0:
      if status == 2: oxyPos = (nx, ny)
      dfs(nx, ny)
      comp.inputs.addLast(opp[cmd])
      comp.run()
      discard comp.outputs.popFirst()

visited[(0, 0)] = true
map[(0, 0)] = 1
dfs(0, 0)

proc solve(start, target: (int, int), part2: bool): int =
  var q = initDeque[((int, int), int)]()
  q.addLast((start, 0))
  var seen = initTable[(int, int), bool]()
  seen[start] = true
  var maxD = 0
  while q.len > 0:
    let (pos, d) = q.popFirst()
    if not part2 and pos == target: return d
    if d > maxD: maxD = d
    for i in 1..4:
      let npos = (pos[0] + dx[i], pos[1] + dy[i])
      if map.getOrDefault(npos, 0) != 0 and npos notin seen:
        seen[npos] = true
        q.addLast((npos, d + 1))
  return maxD

echo solve((0, 0), oxyPos, false)
echo solve(oxyPos, (0, 0), true)

