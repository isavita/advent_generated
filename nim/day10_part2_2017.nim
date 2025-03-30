
import std / [strutils, sequtils, strformat]

proc knotHash(inputStr: string, rounds = 64): string =
  var list = newSeq[uint8](256)
  for i in 0..255: list[i] = uint8(i)

  var lengths = newSeq[int]()
  for c in inputStr: lengths.add(ord(c))
  lengths.add(@[17, 31, 73, 47, 23])

  var pos = 0
  var skip = 0
  let listLen = list.len

  for round in 1..rounds:
    for length in lengths:
      if length > 1:
        for i in 0 ..< length div 2:
          let idx1 = (pos + i) mod listLen
          let idx2 = (pos + length - 1 - i) mod listLen
          swap(list[idx1], list[idx2])

      pos = (pos + length + skip) mod listLen
      inc skip

  var denseHash = newSeq[uint8]()
  for i in countup(0, listLen - 1, 16):
    var xorSum: uint8 = 0
    for j in i ..< i + 16:
      xorSum = xorSum xor list[j]
    denseHash.add(xorSum)

  result = denseHash.map(proc(num: uint8): string = fmt"{num:02x}").join("")

when isMainModule:
  let puzzleInput = readFile("input.txt").strip()
  echo knotHash(puzzleInput)
