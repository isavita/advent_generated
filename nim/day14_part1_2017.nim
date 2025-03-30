
import strutils, sequtils, os, bitops

const ListSize = 256

proc knotHash(input: string): seq[uint8] =
  var lengths: seq[int]
  for c in input:
    lengths.add(int(c))
  lengths.add(@[17, 31, 73, 47, 23])

  var nums: array[ListSize, uint8]
  for i in 0 ..< ListSize:
    nums[i] = uint8(i)

  var pos = 0
  var skip = 0

  for round in 1 .. 64:
    for length in lengths:
      if length > 1:
        for i in 0 ..< (length div 2):
          let a = (pos + i) mod ListSize
          let b = (pos + length - 1 - i) mod ListSize
          swap(nums[a], nums[b])
      
      pos = (pos + length + skip) # No explicit modulo on pos update itself, like Python
      inc skip
      # Nim's mod handles negative results differently than Python's %, 
      # but pos >= 0 here. The indices a & b use mod correctly.
      # Keeping pos potentially large matches Python's behavior.

  result = newSeq[uint8](16)
  for i in 0 ..< 16:
    var xored = nums[i * 16] 
    for j in 1 ..< 16:
      xored = xored xor nums[i * 16 + j]
    result[i] = xored

when isMainModule:
  let key = readFile("input.txt").strip()
  var totalSetBits = 0

  for i in 0 ..< 128:
    let inputStr = key & "-" & $i
    let denseHash = knotHash(inputStr)
    for byteVal in denseHash:
      totalSetBits += countSetBits(byteVal)

  echo totalSetBits
