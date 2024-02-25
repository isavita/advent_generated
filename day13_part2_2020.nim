
import os
import strutils

proc readBusIDs(fileName: string): (seq[int], seq[int]) =
  var ids: seq[int] = @[]
  var offsets: seq[int] = @[]
  let file = open(fileName, fmRead)
  discard file.readLine()  # Skip the first line
  let busData = file.readLine().split(',')

  for i, bus in busData.pairs:
    if bus != "x":
      ids.add(bus.parseInt())
      offsets.add(i)

  (ids, offsets)  # Return tuple directly

proc extendedGCD(a, b: int): (int, int) =
  if a == 0:
    return (0, 1)
  let (x1, y1) = extendedGCD(b mod a, a)
  let x = y1 - (b div a) * x1
  let y = x1
  (x, y)  # Return tuple directly

proc findEarliestTimestamp(ids, offsets: seq[int]): int =
  var N = 1
  for id in ids:
    N *= id

  var result = 0
  for i, id in ids.pairs:
    let ni = N div id
    let (xi, _) = extendedGCD(ni, id)
    result += (-offsets[i] + id) mod id * xi * ni

  result mod N

let (ids, offsets) = readBusIDs("input.txt")
let timestamp = findEarliestTimestamp(ids, offsets)
echo timestamp
