
import std/strutils
import std/tables
import std/sequtils
import std/math

const
  STEPS = 40
  FILENAME = "input.txt"

proc solve() =
  let content = readFile(FILENAME)
  let lines = content.splitLines()

  if lines.len == 0: return

  let templateStr = lines[0]
  var rules = initTable[string, char]()

  for line in lines[2..^1]:
    let lineStrip = line.strip()
    if lineStrip.len > 0:
      let parts = lineStrip.split(" -> ")
      rules[parts[0]] = parts[1][0]

  var pairCounts = initTable[string, int64]()
  var charCounts = initTable[char, int64]()

  for i in 0..<templateStr.len - 1:
    let pair = templateStr[i..i+1]
    pairCounts[pair] = pairCounts.getOrDefault(pair, 0) + 1

  for c in templateStr:
    charCounts[c] = charCounts.getOrDefault(c, 0) + 1

  for step in 1..STEPS:
    var newPairCounts = initTable[string, int64]()

    for pair, count in pairCounts.pairs:
      if rules.contains(pair):
        let insertChar = rules[pair]

        let newPair1 = pair[0] & insertChar
        let newPair2 = insertChar & pair[1]

        newPairCounts[newPair1] = newPairCounts.getOrDefault(newPair1, 0) + count
        newPairCounts[newPair2] = newPairCounts.getOrDefault(newPair2, 0) + count

        charCounts[insertChar] = charCounts.getOrDefault(insertChar, 0) + count

    pairCounts = newPairCounts

  var maxCount: int64 = 0
  var minCount: int64 = high(int64)

  if charCounts.len > 0:
    for _, count in charCounts.pairs:
      if count > maxCount:
        maxCount = count
      if count < minCount:
        minCount = count

    echo maxCount - minCount

when isMainModule:
  solve()
