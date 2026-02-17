
import strutils, algorithm, sequtils

type Monkey = object
  items: seq[int64]
  opChar: char
  opVal: int64
  divVal: int64
  nextTrue, nextFalse: int
  inspections: int64

proc solve() =
  let content = readFile("input.txt").replace("\r\n", "\n").strip()
  let blocks = content.split("\n\n")
  var monkeys: seq[Monkey] = @[]

  for b in blocks:
    let lines = b.splitLines()
    var m: Monkey
    m.items = lines[1].split(": ")[1].split(", ").mapIt(it.strip.parseBiggestInt)
    let ops = lines[2].split("= ")[1].split(" ")
    m.opChar = ops[1][0]
    m.opVal = if ops[2] == "old": -1 else: ops[2].parseBiggestInt
    m.divVal = lines[3].split("by ")[1].parseBiggestInt
    m.nextTrue = lines[4].split("monkey ")[1].parseInt
    m.nextFalse = lines[5].split("monkey ")[1].parseInt
    monkeys.add(m)

  var commonMultiple: int64 = 1
  for m in monkeys: commonMultiple *= m.divVal

  for round in 1..10000:
    for i in 0..<monkeys.len:
      let items = monkeys[i].items
      monkeys[i].items = @[]
      for item in items:
        monkeys[i].inspections.inc
        let val = if monkeys[i].opVal == -1: item else: monkeys[i].opVal
        var worry = if monkeys[i].opChar == '+': item + val else: item * val
        worry = worry mod commonMultiple
        if worry mod monkeys[i].divVal == 0:
          monkeys[monkeys[i].nextTrue].items.add(worry)
        else:
          monkeys[monkeys[i].nextFalse].items.add(worry)

  var counts = monkeys.mapIt(it.inspections)
  counts.sort(Descending)
  echo counts[0] * counts[1]

solve()

