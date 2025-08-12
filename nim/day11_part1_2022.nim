import std/[os, strutils, sequtils, parseutils, algorithm]

type
  Monkey = object
    items: seq[int64]
    operation: char
    operand: int64
    testDivisor: int
    trueMonkey: int
    falseMonkey: int
    inspectCount: int64

proc performOperation(oldValue: int64, op: char, operand: int64): int64 =
  case op
  of '+': oldValue + operand
  of '*':
    if operand == -1: oldValue * oldValue
    else: oldValue * operand
  else: oldValue

proc readMonkeys(filename: string): seq[Monkey] =
  var lines = readFile(filename).splitLines()
  var i = 0
  var monkeys: seq[Monkey] = @[]
  while i < lines.len:
    if lines[i].startsWith("Monkey"):
      var items: seq[int64] = @[]
      i += 1
      var parts = lines[i].strip().split(":")[1].strip().split(", ")
      for p in parts:
        items.add parseInt(p)
      i += 1
      var opParts = lines[i].strip().split(" ")
      let operation = opParts[4][0]
      var operand: int64
      try:
        operand = parseInt(opParts[5])
      except ValueError:
        operand = -1
      i += 1
      let testDivisor = parseInt(lines[i].strip().split(" ")[3])
      i += 1
      let trueMonkey = parseInt(lines[i].strip().split(" ")[5])
      i += 1
      let falseMonkey = parseInt(lines[i].strip().split(" ")[5])
      monkeys.add Monkey(items: items, operation: operation, operand: operand,
                         testDivisor: testDivisor, trueMonkey: trueMonkey,
                         falseMonkey: falseMonkey, inspectCount: 0)
      i += 2
    else:
      i += 1
  monkeys

proc main =
  var monkeys = readMonkeys("input.txt")
  for round in 0..<20:
    for mi in 0..<monkeys.len:
      var m = monkeys[mi]
      for item in m.items:
        monkeys[mi].inspectCount += 1
        var newValue = performOperation(item, m.operation, m.operand) div 3
        let target = if newValue mod m.testDivisor == 0: m.trueMonkey else: m.falseMonkey
        monkeys[target].items.add(newValue)
      monkeys[mi].items.setLen(0)
  var inspectCounts = monkeys.mapIt(it.inspectCount)
  inspectCounts.sort()
  let n = inspectCounts.len
  let monkeyBusiness = inspectCounts[n-1] * inspectCounts[n-2]
  echo monkeyBusiness

when isMainModule:
  main()