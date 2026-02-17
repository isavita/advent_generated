
import std/[os, strutils, tables, deques, math]

type
  ModuleKind = enum mkBroadcaster, mkFlipFlop, mkConjunction
  Module = ref object
    kind: ModuleKind
    name: string
    dests: seq[string]
    ffState: bool
    conjMem: Table[string, bool]

type Signal = object
  src, dest: string
  pulse: bool

proc gcd(a, b: int64): int64 =
  if b == 0: a else: gcd(b, a mod b)

proc lcm(a, b: int64): int64 =
  if a == 0 or b == 0: 0 else: abs(a * b) div gcd(a, b)

var modules = initTable[string, Module]()
let lines = readFile("input.txt").strip().splitLines()

for line in lines:
  let parts = line.split(" -> ")
  let dests = parts[1].split(", ")
  var name = parts[0]
  var kind: ModuleKind
  if name == "broadcaster":
    kind = mkBroadcaster
  elif name.startsWith("%"):
    kind = mkFlipFlop
    name = name[1..^1]
  else:
    kind = mkConjunction
    name = name[1..^1]
  modules[name] = Module(kind: kind, name: name, dests: dests, ffState: false, conjMem: initTable[string, bool]())

for name, m in modules:
  for d in m.dests:
    if modules.contains(d) and modules[d].kind == mkConjunction:
      modules[d].conjMem[name] = false

var rxFeeder = ""
for name, m in modules:
  for d in m.dests:
    if d == "rx": rxFeeder = name

var loopLengths = initTable[string, int64]()
for name, m in modules:
  for d in m.dests:
    if d == rxFeeder: loopLengths[name] = -1

var pressCount: int64 = 0
var q = initDeque[Signal]()

block simulation:
  while true:
    pressCount += 1
    q.addLast(Signal(src: "button", dest: "broadcaster", pulse: false))
    while q.len > 0:
      let s = q.popFirst()
      if not modules.contains(s.dest): continue
      let m = modules[s.dest]
      var outPulse = false
      var send = false

      case m.kind:
      of mkBroadcaster:
        outPulse = s.pulse
        send = true
      of mkFlipFlop:
        if not s.pulse:
          m.ffState = not m.ffState
          outPulse = m.ffState
          send = true
      of mkConjunction:
        m.conjMem[s.src] = s.pulse
        var allHigh = true
        for v in m.conjMem.values:
          if not v:
            allHigh = false
            break
        outPulse = not allHigh
        send = true
        if outPulse and loopLengths.contains(m.name) and loopLengths[m.name] == -1:
          loopLengths[m.name] = pressCount
      
      if send:
        for d in m.dests:
          q.addLast(Signal(src: m.name, dest: d, pulse: outPulse))
    
    var allFound = true
    for v in loopLengths.values:
      if v == -1:
        allFound = false
        break
    if allFound: break simulation

var ans: int64 = 1
for v in loopLengths.values:
  ans = lcm(ans, v)

echo ans
