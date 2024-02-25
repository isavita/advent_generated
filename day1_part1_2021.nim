
import std/[os, strutils]

proc countDepthIncreases(inputFile: string): int =
  let inputLines = readFile(inputFile).strip().splitlines()
  var prevMeasurement = parseInt(inputLines[0])
  result = 0
  for i in 1..<len(inputLines):
    let measurement = parseInt(inputLines[i])
    if measurement > prevMeasurement:
      result += 1
    prevMeasurement = measurement

when isMainModule:
  countDepthIncreases("input.txt").echo()
