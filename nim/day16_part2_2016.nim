
import std/[strutils, sequtils]

const diskLength = 35651584

proc readInitialState(filename: string): string =
  readFile(filename).strip()

proc generateData(initialState: string, length: int): string =
  var data = initialState
  while data.len < length:
    var b = ""
    for i in countdown(data.len - 1, 0):
      b.add(if data[i] == '0': '1' else: '0')
    data = data & "0" & b
  data[0..<length]

proc calculateChecksum(data: string): string =
  var current = data
  while current.len mod 2 == 0:
    var next = ""
    for i in countup(0, current.len - 1, 2):
      next.add(if current[i] == current[i+1]: '1' else: '0')
    current = next
  current

proc main() =
  let initialState = readInitialState("input.txt")
  let data = generateData(initialState, diskLength)
  let checksum = calculateChecksum(data)
  echo "Checksum: ", checksum

main()
