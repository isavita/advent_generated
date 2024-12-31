
import strutils, sequtils, os

proc nextSecret(s: uint64): uint64 =
  var x = s * 64
  result = s xor x
  result = result and 0xFFFFFF
  x = result div 32
  result = result xor x
  result = result and 0xFFFFFF
  x = result * 2048
  result = result xor x
  result = result and 0xFFFFFF

var buyers: seq[uint64] = @[]
for line in lines("input.txt"):
  if line.len > 0:
    buyers.add(parseUInt(line))

var total: uint64 = 0
for b in buyers:
  var s = b
  for i in 0..<2000:
    s = nextSecret(s)
  total += s

echo total
