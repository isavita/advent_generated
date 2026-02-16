import std/[sequtils, strutils, streams, os, tables]

type
  Intcode = object
    mem: Table[int, int]
    ip: int
    input: seq[int]
    output: int
    halted: bool

proc newIntcode(prog: seq[int]; initInput: seq[int]): Intcode =
  var t = initTable[int, int]()
  for i, v in prog: t[i] = v
  Intcode(mem: t, ip: 0, input: initInput, output: 0, halted: false)

proc addInput(c: var Intcode; v: int) = c.input.add v

proc getMem(c: var Intcode; a: int): int =
  if a in c.mem: c.mem[a] else: 0

proc setMem(c: var Intcode; a, v: int) = c.mem[a] = v

proc run(c: var Intcode) =
  while not c.halted:
    let instr = c.getMem(c.ip)
    let opcode = instr mod 100
    let mode1 = (instr div 100) mod 10
    let mode2 = (instr div 1000) mod 10
    template param(i, mode): int =
      if mode == 1: c.getMem(c.ip + i)
      else: c.getMem(c.getMem(c.ip + i))
    case opcode
    of 1:
      let p1 = param(1, mode1)
      let p2 = param(2, mode2)
      c.setMem(c.getMem(c.ip + 3), p1 + p2)
      c.ip += 4
    of 2:
      let p1 = param(1, mode1)
      let p2 = param(2, mode2)
      c.setMem(c.getMem(c.ip + 3), p1 * p2)
      c.ip += 4
    of 3:
      c.setMem(c.getMem(c.ip + 1), c.input[0])
      c.input = c.input[1..^1]
      c.ip += 2
    of 4:
      c.output = c.getMem(c.getMem(c.ip + 1))
      c.ip += 2
      return
    of 5:
      if c.getMem(c.getMem(c.ip + 1)) != 0: c.ip = c.getMem(c.getMem(c.ip + 2))
      else: c.ip += 3
    of 6:
      if c.getMem(c.getMem(c.ip + 1)) == 0: c.ip = c.getMem(c.getMem(c.ip + 2))
      else: c.ip += 3
    of 7:
      let v = if c.getMem(c.getMem(c.ip + 1)) < c.getMem(c.getMem(c.ip + 2)): 1 else: 0
      c.setMem(c.getMem(c.ip + 3), v)
      c.ip += 4
    of 8:
      let v = if c.getMem(c.getMem(c.ip + 1)) == c.getMem(c.getMem(c.ip + 2)): 1 else: 0
      c.setMem(c.getMem(c.ip + 3), v)
      c.ip += 4
    of 99:
      c.halted = true
      return
    else: c.halted = true; return

proc permutations(arr: seq[int]): seq[seq[int]] =
  if arr.len == 0: @[ @[] ]
  else:
    var res: seq[seq[int]] = @[]
    for i in 0..<arr.len:
      let rest = arr[0..<i] & arr[i+1..<arr.len]
      for p in permutations(rest):
        res.add @[arr[i]] & p
    res

let prog = readFile("input.txt").split(',').mapIt(it.strip.parseInt)
let phaseSeq = @[5,6,7,8,9]
var best = 0
for perm in permutations(phaseSeq):
  var amps: array[5, Intcode]
  for i in 0..<5:
    amps[i] = newIntcode(prog, @[perm[i]])
  var signal = 0
  while not amps[4].halted:
    for i in 0..<5:
      amps[i].addInput(signal)
      amps[i].run()
      signal = amps[i].output
  if signal > best: best = signal
echo best