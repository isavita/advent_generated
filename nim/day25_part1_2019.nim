# day25_part1_2019.nim
# AoC 2019 Day 25 (Intcode text adventure) automated solver in Nim 2.0+
#
# Compile:
#   nim c -d:release day25_part1_2019.nim
# Run:
#   ./day25_part1_2019 input.txt
#
# Fix for your error: 'out' is a keyword in Nim, so we don't use it as a field name.

import std/[strutils, sequtils, tables, sets, deques, algorithm, os, re, options]

# ----------------------------- Intcode Emulator -----------------------------

type
  EmuStatus = enum
    esHalted, esOutput, esWaiting

  EmuStep = object
    status: EmuStatus
    outVal: int64          # valid iff status == esOutput

  Emulator = ref object
    mem: Table[int64, int64]
    input: Deque[int64]
    ip: int64
    rb: int64

proc newEmulator(program: seq[int64]): Emulator =
  new(result)
  result.mem = initTable[int64, int64](program.len * 2 + 16)
  for i, v in program:
    result.mem[int64(i)] = v
  result.input = initDeque[int64]()
  result.ip = 0
  result.rb = 0

proc memGet(e: Emulator; address: int64): int64 =
  if e.mem.hasKey(address): e.mem[address] else: 0

proc memSet(e: Emulator; address, v: int64) =
  e.mem[address] = v

proc writeString(e: Emulator; s: string) =
  for ch in s:
    e.input.addLast(int64(ord(ch)))

proc pow10(n: int): int64 =
  var p = 1'i64
  for _ in 1..n: p *= 10
  p

proc modeOf(instr: int64; offset: int): int64 =
  # offset is 1-based param index; mode digit at 10^(offset+1)
  (instr div pow10(offset + 1)) mod 10

proc getParam(e: Emulator; instr: int64; offset: int): int64 =
  let mode = modeOf(instr, offset)
  let param = memGet(e, e.ip + int64(offset))
  case mode
  of 0: memGet(e, param)
  of 1: param
  of 2: memGet(e, e.rb + param)
  else: raise newException(ValueError, "Unknown parameter mode: " & $mode)

proc getWriteAddr(e: Emulator; instr: int64; offset: int): int64 =
  let mode = modeOf(instr, offset)
  let param = memGet(e, e.ip + int64(offset))
  case mode
  of 0: param
  of 2: e.rb + param
  else: raise newException(ValueError, "Invalid mode for writing: " & $mode)

proc emulate(e: Emulator): EmuStep =
  while true:
    let instr = memGet(e, e.ip)
    let opcode = instr mod 100

    case opcode
    of 1:
      let a = getParam(e, instr, 1)
      let b = getParam(e, instr, 2)
      let c = getWriteAddr(e, instr, 3)
      memSet(e, c, a + b)
      e.ip += 4
    of 2:
      let a = getParam(e, instr, 1)
      let b = getParam(e, instr, 2)
      let c = getWriteAddr(e, instr, 3)
      memSet(e, c, a * b)
      e.ip += 4
    of 3:
      if e.input.len == 0:
        return EmuStep(status: esWaiting)
      let c = getWriteAddr(e, instr, 1)
      memSet(e, c, e.input.popFirst())
      e.ip += 2
    of 4:
      let a = getParam(e, instr, 1)
      e.ip += 2
      return EmuStep(status: esOutput, outVal: a)
    of 5:
      let a = getParam(e, instr, 1)
      let b = getParam(e, instr, 2)
      e.ip = (if a != 0: b else: e.ip + 3)
    of 6:
      let a = getParam(e, instr, 1)
      let b = getParam(e, instr, 2)
      e.ip = (if a == 0: b else: e.ip + 3)
    of 7:
      let a = getParam(e, instr, 1)
      let b = getParam(e, instr, 2)
      let c = getWriteAddr(e, instr, 3)
      memSet(e, c, (if a < b: 1 else: 0))
      e.ip += 4
    of 8:
      let a = getParam(e, instr, 1)
      let b = getParam(e, instr, 2)
      let c = getWriteAddr(e, instr, 3)
      memSet(e, c, (if a == b: 1 else: 0))
      e.ip += 4
    of 9:
      let a = getParam(e, instr, 1)
      e.rb += a
      e.ip += 2
    of 99:
      return EmuStep(status: esHalted)
    else:
      raise newException(ValueError, "Unknown opcode: " & $opcode & " at ip " & $e.ip)

# ----------------------------- World / Solver State -----------------------------

type
  Mode = enum
    mExplore, mNavigate, mTest

  Room = ref object
    name: string
    # dir -> destination room name, or none for unexplored
    conns: Table[string, Option[string]]

  State = ref object
    emu: Emulator
    world: Table[string, Room]
    inv: Table[string, bool]

    mode: Mode
    currentRoom: Option[string]

    checkpoint: Option[string]
    floorRoom: Option[string]
    testDir: string

    # Explore: stack (push/pop at end). Navigate: queue stored as seq (pop from front).
    path: seq[string]

    availableItems: seq[string]
    itemMask: int

    lastRoom: Option[string]
    lastItems: seq[string]
    lastDir: string

    outBuf: string

let opposite = {"north":"south", "south":"north", "west":"east", "east":"west"}.toTable
let blacklist = ["photons", "escape pod", "molten lava", "infinite loop", "giant electromagnet"].toHashSet

proc getRoom(st: State; name: string): Room =
  if st.world.hasKey(name):
    st.world[name]
  else:
    var r = Room(name: name, conns: initTable[string, Option[string]]())
    st.world[name] = r
    r

proc ensureRoom(st: State; name: string) =
  discard getRoom(st, name)

proc setDoor(st: State; dir: string) =
  if st.currentRoom.isSome:
    let rn = st.currentRoom.get()
    let r = getRoom(st, rn)
    if not r.conns.hasKey(dir):
      r.conns[dir] = none(string)

proc updateConnections(st: State) =
  if st.lastRoom.isSome and st.currentRoom.isSome and st.lastDir.len > 0:
    let lastN = st.lastRoom.get()
    let curN  = st.currentRoom.get()
    let lastR = getRoom(st, lastN)
    let curR  = getRoom(st, curN)

    var shouldLink = true
    if lastR.conns.hasKey(st.lastDir):
      if lastR.conns[st.lastDir].isSome:
        shouldLink = false

    if shouldLink:
      lastR.conns[st.lastDir] = some(curN)
      curR.conns[opposite[st.lastDir]] = some(lastN)

proc handleAlert(st: State) =
  if st.mode == mExplore:
    if st.path.len > 0:
      st.path.setLen(st.path.len - 1)

    st.checkpoint = st.lastRoom
    st.floorRoom = st.currentRoom
    st.testDir = st.lastDir

    if st.checkpoint.isSome and st.floorRoom.isSome and st.testDir.len > 0:
      let cp = getRoom(st, st.checkpoint.get())
      cp.conns[st.testDir] = some(st.floorRoom.get())

  st.lastRoom = none(string)
  st.lastItems = @[]
  st.lastDir = ""

proc tryDirectionTo(st: State; fromName, toName: string): Option[string] =
  let r = getRoom(st, fromName)
  for dir, dest in r.conns.pairs:
    if dest.isSome and dest.get() == toName:
      return some(dir)
  none(string)

proc chooseUnexplored(st: State; roomName: string): Option[string] =
  let r = getRoom(st, roomName)
  for d in ["north","south","west","east"]:
    if r.conns.hasKey(d) and r.conns[d].isNone:
      return some(d)
  for dir, dest in r.conns.pairs:
    if dest.isNone: return some(dir)
  none(string)

proc findPath(st: State; fromName, toName: string): Option[seq[string]] =
  var q = initDeque[(string, seq[string])]()
  q.addLast((fromName, @[fromName]))
  var visited = initHashSet[string]()
  visited.incl(fromName)

  while q.len > 0:
    let (node, path) = q.popFirst()
    if node == toName: return some(path)
    let r = getRoom(st, node)
    for _, dest in r.conns.pairs:
      if dest.isSome:
        let n = dest.get()
        if not visited.contains(n):
          visited.incl(n)
          q.addLast((n, path & @[n]))
  none(seq[string])

proc sendCommand(st: State; cmd: string) =
  writeString(st.emu, cmd)

proc bootstrapMove(st: State) =
  st.lastDir = "north"
  sendCommand(st, "north\n")

# ----------------------------- Output Parsing -----------------------------

proc processOutput(st: State; output: string): seq[string] =
  var items: seq[string] = @[]
  let lines = output.splitLines()

  var i = 0
  while i < lines.len:
    let line = lines[i].strip()
    if line.len == 0 or line == "Command?":
      inc i
      continue

    if line.startsWith("== ") and line.endsWith(" ==") and line.len >= 6:
      let name = line[3 ..< line.len-3].strip()
      ensureRoom(st, name)
      st.currentRoom = some(name)
      items = @[]
      inc i
      while i < lines.len and lines[i].strip().len != 0: inc i
      continue

    if line == "Doors here lead:":
      inc i
      while i < lines.len and lines[i].strip().len != 0:
        let dl = lines[i].strip()
        if dl.startsWith("- "):
          setDoor(st, dl[2 .. ^1])
        inc i
      continue

    if line == "Items here:":
      inc i
      items = @[]
      while i < lines.len and lines[i].strip().len != 0:
        let il = lines[i].strip()
        if il.startsWith("- "):
          items.add(il[2 .. ^1])
        inc i
      continue

    if line.startsWith("You take the ") and line.endsWith("."):
      let taken = line["You take the ".len ..< line.len-1]
      st.inv[taken] = true
      if st.lastRoom.isSome:
        st.currentRoom = st.lastRoom
        items = st.lastItems.filterIt(it != taken)
      inc i
      continue

    if line.startsWith("You drop the ") and line.endsWith("."):
      let dropped = line["You drop the ".len ..< line.len-1]
      st.inv[dropped] = false
      if st.lastRoom.isSome:
        st.currentRoom = st.lastRoom
        items = st.lastItems & @[dropped]
      inc i
      continue

    if line.startsWith("A loud, robotic voice says \"Alert!"):
      handleAlert(st)
      inc i
      continue

    inc i

  items

proc extractCode(output: string): Option[string] =
  let rx = re"Oh, hello! You should be able to get in by typing (\d+) on the keypad at the main airlock\."
  var matches: array[1, string]
  if output.find(rx, matches) >= 0:
    return some(matches[0])
  none(string)

# ----------------------------- Decision Logic -----------------------------

proc exploreStep(st: State; items: seq[string]) =
  for it in items:
    if not blacklist.contains(it):
      sendCommand(st, "take " & it & "\n")
      return

  if st.currentRoom.isNone:
    bootstrapMove(st)
    return

  let cur = st.currentRoom.get()
  let dirOpt = chooseUnexplored(st, cur)
  if dirOpt.isSome:
    let dir = dirOpt.get()
    st.path.add(cur)   # push (stack end)
    st.lastDir = dir
    sendCommand(st, dir & "\n")
    return

  if st.path.len > 0:
    let prev = st.path[^1]
    st.path.setLen(st.path.len - 1)
    let backOpt = tryDirectionTo(st, cur, prev)
    if backOpt.isNone:
      raise newException(ValueError, "Cannot go from \"" & cur & "\" to \"" & prev & "\"")
    let back = backOpt.get()
    st.lastDir = back
    sendCommand(st, back & "\n")
    return

  if st.checkpoint.isSome and st.floorRoom.isSome:
    let cp = st.checkpoint.get()
    let pOpt = findPath(st, cur, cp)
    if pOpt.isNone: raise newException(ValueError, "No path to checkpoint")
    let p = pOpt.get()
    st.path = (if p.len > 1: p[1 .. ^1] else: @[])  # drop current
    st.mode = mNavigate
    return

  raise newException(ValueError, "No checkpoint found")

proc navigateStep(st: State) =
  if st.currentRoom.isNone:
    bootstrapMove(st)
    return

  let cur = st.currentRoom.get()
  if st.path.len == 0:
    var avail: seq[string] = @[]
    for k, v in st.inv.pairs:
      if v: avail.add(k)
    avail.sort()
    st.availableItems = avail
    st.itemMask = 0
    st.mode = mTest
    return

  let nxt = st.path[0]
  st.path = st.path[1 .. ^1]
  let dirOpt = tryDirectionTo(st, cur, nxt)
  if dirOpt.isNone:
    raise newException(ValueError, "Cannot go from \"" & cur & "\" to \"" & nxt & "\"")
  let dir = dirOpt.get()
  st.lastDir = dir
  sendCommand(st, dir & "\n")

proc testStep(st: State) =
  let items = st.availableItems
  let mask = st.itemMask
  let maxMask = 1 shl items.len
  if mask >= maxMask:
    raise newException(ValueError, "No valid item combination found after " & $maxMask & " attempts")

  for idx, item in items:
    let target = (mask and (1 shl idx)) != 0
    let cur = st.inv.getOrDefault(item, false)
    if cur != target:
      let action = (if target: "take " else: "drop ")
      sendCommand(st, action & item & "\n")
      return

  st.itemMask = mask + 1
  if st.testDir.len == 0:
    raise newException(ValueError, "Test direction not set")
  sendCommand(st, st.testDir & "\n")

# ----------------------------- Main Loop -----------------------------

proc main() =
  let filename = if paramCount() >= 1: paramStr(1) else: "input.txt"
  let text = readFile(filename).strip()
  let program = text.split(',').mapIt(parseBiggestInt(it.strip()).int64)

  let st = State(
    emu: newEmulator(program),
    world: initTable[string, Room](),
    inv: initTable[string, bool](),
    mode: mExplore,
    currentRoom: none(string),
    checkpoint: none(string),
    floorRoom: none(string),
    testDir: "",
    path: @[],
    availableItems: @[],
    itemMask: 0,
    lastRoom: none(string),
    lastItems: @[],
    lastDir: "",
    outBuf: ""
  )

  var actions = 0
  let maxActions = 30000

  while true:
    let step = emulate(st.emu)
    case step.status
    of esOutput:
      st.outBuf.add(char(step.outVal.int))
    of esHalted:
      let outStr = st.outBuf
      let codeOpt = extractCode(outStr)
      if codeOpt.isSome:
        echo codeOpt.get()
        quit(0)
      else:
        stderr.writeLine("Error: No solution found")
        stderr.writeLine(outStr)
        quit(1)
    of esWaiting:
      inc actions
      if actions > maxActions:
        raise newException(ValueError, "Loop detected (action cap reached)")

      let outStr = st.outBuf
      st.outBuf = ""

      let items = processOutput(st, outStr)
      updateConnections(st)

      st.lastRoom = st.currentRoom
      st.lastItems = items
      st.lastDir = ""

      case st.mode
      of mExplore: exploreStep(st, items)
      of mNavigate: navigateStep(st)
      of mTest: testStep(st)

when isMainModule:
  main()

