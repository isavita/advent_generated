
import strutils
import os

proc processBlock(lines: seq[string], s, e: int): int64 =
  var nums: seq[int64] = @[]
  var op: char = '+'
  for c in s..e:
    var buf = ""
    for r, line in lines:
      if c < line.len:
        let ch = line[c]
        if ch in '0'..'9': buf.add ch
        elif ch in ['+', '*']: op = ch
    if buf.len > 0: nums.add buf.parseInt.int64
  if nums.len == 0: return 0
  var blockRes: int64 = if op == '*': 1 else: 0
  for num in nums:
    blockRes = if op == '*': blockRes * num else: blockRes + num
  result += blockRes

proc main() =
  let data = readFile("input.txt")
  let lines = data.splitLines()
  let linecnt = lines.len
  if linecnt == 0:
    echo "Grand total: 0"
    return

  var maxw = 0
  for line in lines: maxw = max(maxw, line.len)

  var isSep = newSeq[bool](maxw)
  for i in 0..<maxw: isSep[i] = true
  for x in 0..<maxw:
    for r, line in lines:
      let ch = if x < line.len: line[x] else: ' '
      if not ch.isSpaceAscii: 
        isSep[x] = false
        break

  var grandTotal: int64 = 0
  var inBlock = false
  var start = 0
  for x in 0..<maxw:
    if not isSep[x]:
      if not inBlock: 
        inBlock = true
        start = x
    else:
      if inBlock:
        grandTotal += processBlock(lines, start, x - 1)
        inBlock = false
  if inBlock: grandTotal += processBlock(lines, start, maxw - 1)

  echo "Grand total: ", grandTotal

when isMainModule:
  main()
