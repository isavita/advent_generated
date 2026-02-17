import tables, strutils, deques

type VM = ref object
  code: Table[int64, int64]
  ip, rb: int64
  input: Deque[int64]
  output: seq[int64]

proc gv(v: VM, a: int64): int64 = v.code.getOrDefault(a, 0)

proc gp(v: VM, i: int, m: int64): int64 =
  let p = v.gv(v.ip + i)
  if m == 0: v.gv(p) elif m == 1: p else: v.gv(v.rb + p)

proc ga(v: VM, i: int, m: int64): int64 =
  let p = v.gv(v.ip + i)
  if m == 0: p else: v.rb + p

proc run(v: VM) =
  while true:
    let ins = v.gv(v.ip)
    let op = ins mod 100
    let m1 = (ins div 100) mod 10
    let m2 = (ins div 1000) mod 10
    let m3 = (ins div 10000) mod 10
    case op:
    of 1:
      v.code[v.ga(3, m3)] = v.gp(1, m1) + v.gp(2, m2)
      v.ip += 4
    of 2:
      v.code[v.ga(3, m3)] = v.gp(1, m1) * v.gp(2, m2)
      v.ip += 4
    of 3:
      v.code[v.ga(1, m1)] = v.input.popFirst()
      v.ip += 2
    of 4:
      v.output.add(v.gp(1, m1))
      v.ip += 2
    of 5:
      v.ip = if v.gp(1, m1) != 0: v.gp(2, m2) else: v.ip + 3
    of 6:
      v.ip = if v.gp(1, m1) == 0: v.gp(2, m2) else: v.ip + 3
    of 7:
      v.code[v.ga(3, m3)] = if v.gp(1, m1) < v.gp(2, m2): 1 else: 0
      v.ip += 4
    of 8:
      v.code[v.ga(3, m3)] = if v.gp(1, m1) == v.gp(2, m2): 1 else: 0
      v.ip += 4
    of 9:
      v.rb += v.gp(1, m1)
      v.ip += 2
    of 99: break
    else: break

let content = readFile("input.txt").strip().split(',')
var vm = VM(code: initTable[int64, int64](), input: initDeque[int64](), output: @[])
for i, s in content: vm.code[i.int64] = s.parseBiggestInt

let cmds = ["NOT A J", "NOT B T", "OR T J", "NOT C T", "OR T J", "AND D J", "NOT A T", "AND A T", "OR E T", "OR H T", "AND T J", "RUN"]
for s in cmds:
  for c in s: vm.input.addLast(c.ord.int64)
  vm.input.addLast(10)

vm.run()
for x in vm.output:
  if x in 0..127: stdout.write(x.char)
  else: echo x