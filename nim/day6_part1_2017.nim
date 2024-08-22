import strutils, sequtils, tables

proc redistribute(banks: var seq[int]) =
  var maxBlocks = banks[0]
  var maxIndex = 0
  for i, blocks in banks:
    if blocks > maxBlocks:
      maxBlocks = blocks
      maxIndex = i
  banks[maxIndex] = 0
  for i in 0..<maxBlocks:
    banks[(maxIndex + i + 1) mod banks.len] += 1

proc findCycles(banks: seq[int]): int =
  var seen = initTable[string, bool]()
  var cycles = 0
  var banksCopy = banks
  while true:
    let config = $banksCopy
    if seen.hasKey(config):
      return cycles
    seen[config] = true
    redistribute(banksCopy)
    cycles += 1

when isMainModule:
  let input = readFile("input.txt").strip().splitWhitespace().mapIt(it.parseInt())
  echo findCycles(input)