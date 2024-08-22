import strutils, sequtils, tables

proc redistribute(banks: var seq[int]) =
  var maxBlocks = banks[0]
  var maxIndex = 0
  for i, blocks in banks:
    if blocks > maxBlocks or (blocks == maxBlocks and i < maxIndex):
      maxBlocks = blocks
      maxIndex = i
  banks[maxIndex] = 0
  for i in 0..<maxBlocks:
    banks[(maxIndex + 1 + i) mod banks.len] += 1

proc main() =
  let input = readFile("input.txt").strip().splitWhitespace().map(parseInt)
  var banks = input.toSeq
  var seen = initTable[seq[int], int]()
  var cycles = 0
  while banks notin seen:
    seen[banks] = cycles
    redistribute(banks)
    cycles += 1
  echo cycles - seen[banks]

main()