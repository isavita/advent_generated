
import os, strutils, tables, deques

type
  Intcode = object
    mem: Table[int64, int64]
    ip, rb: int64
    inputs, outputs: Deque[int64]
    halted: bool

proc getParam(c: var Intcode, mode, addrIdx: int64): int64 =
  let p = c.mem.getOrDefault(addrIdx, 0)
  case mode
  of 0: c.mem.getOrDefault(p, 0)
  of 1: p
  of 2: c.mem.getOrDefault(c.rb + p, 0)
  else: 0

proc setParam(c: var Intcode, mode, addrIdx, val: int64) =
  let p = c.mem.getOrDefault(addrIdx, 0)
  case mode
  of 0: c.mem[p] = val
  of 2: c.mem[c.rb + p] = val
  else: discard

proc run(c: var Intcode) =
  while not c.halted:
    let instr = c.mem.getOrDefault(c.ip, 0)
    let op = instr mod 100
    let m = [(instr div 100) mod 10, (instr div 1000) mod 10, (instr div 10000) mod 10]
    case op
    of 1:
      c.setParam(m[2], c.ip + 3, c.getParam(m[0], c.ip + 1) + c.getParam(m[1], c.ip + 2))
      c.ip += 4
    of 2:
      c.setParam(m[2], c.ip + 3, c.getParam(m[0], c.ip + 1) * c.getParam(m[1], c.ip + 2))
      c.ip += 4
    of 3:
      if c.inputs.len == 0: return
      c.setParam(m[0], c.ip + 1, c.inputs.popFirst())
      c.ip += 2
    of 4:
      c.outputs.addLast(c.getParam(m[0], c.ip + 1))
      c.ip += 2
    of 5:
      if c.getParam(m[0], c.ip + 1) != 0: c.ip = c.getParam(m[1], c.ip + 2)
      else: c.ip += 3
    of 6:
      if c.getParam(m[0], c.ip + 1) == 0: c.ip = c.getParam(m[1], c.ip + 2)
      else: c.ip += 3
    of 7:
      c.setParam(m[2], c.ip + 3, if c.getParam(m[0], c.ip + 1) < c.getParam(m[1], c.ip + 2): 1 else: 0)
      c.ip += 4
    of 8:
      c.setParam(m[2], c.ip + 3, if c.getParam(m[0], c.ip + 1) == c.getParam(m[1], c.ip + 2): 1 else: 0)
      c.ip += 4
    of 9:
      c.rb += c.getParam(m[0], c.ip + 1)
      c.ip += 2
    of 99: c.halted = true
    else: break

proc main() =
  let raw = readFile("input.txt").strip().split(',')
  var baseMem = initTable[int64, int64]()
  for i, v in raw: baseMem[i.int64] = v.parseBiggestInt()

  for part in 1..2:
    var
      c = Intcode(mem: baseMem)
      panels = initTable[(int, int), int]()
      painted = initTable[(int, int), bool]()
      pos = (0, 0)
      dir = 0
      dirs = [(0, -1), (1, 0), (0, 1), (-1, 0)]
    if part == 2: panels[pos] = 1
    while not c.halted:
      c.inputs.addLast(panels.getOrDefault(pos, 0).int64)
      c.run()
      while c.outputs.len >= 2:
        let color = c.outputs.popFirst()
        let turn = c.outputs.popFirst()
        panels[pos] = color.int
        painted[pos] = true
        dir = if turn == 0: (dir + 3) mod 4 else: (dir + 1) mod 4
        pos = (pos[0] + dirs[dir][0], pos[1] + dirs[dir][1])
    if part == 1:
      echo "Part 1: ", painted.len
    else:
      echo "Part 2:"
      var minX, maxX, minY, maxY: int
      for p in panels.keys:
        minX = min(minX, p[0]); maxX = max(maxX, p[0])
        minY = min(minY, p[1]); maxY = max(maxY, p[1])
      for y in minY..maxY:
        var row = ""
        for x in minX..maxX: row.add(if panels.getOrDefault((x, y), 0) == 1: "█" else: " ")
        echo row

main()

