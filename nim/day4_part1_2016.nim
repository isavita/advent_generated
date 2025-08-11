import os, strutils, sequtils, algorithm

proc isRealRoom(room: string): bool =
  let parts = room.split("[")
  let checksum = parts[1][0..^2] # remove trailing ']'
  var nameParts = parts[0].split('-')
  nameParts.setLen(nameParts.len - 1) # drop sector id part
  var counts = newSeq[int](26)
  for part in nameParts:
    for ch in part:
      counts[ord(ch) - ord('a')] += 1
  var letters: seq[(char, int)] = @[]
  for i in 0..<26:
    if counts[i] > 0:
      letters.add((chr(ord('a') + i, ), counts[i]))
  sort(letters, proc(a, b: (char, int)): int =
    if a[1] != b[1]: b[1] - a[1] else: ord(a[0]) - ord(b[0]))
  for i in 0..<checksum.len:
    if checksum[i] != letters[i][0]:
      return false
  true

proc getSectorID(room: string): int =
  let parts = room.split('-')
  let sectorPart = parts[^1]
  result = parseInt(sectorPart.split('[')[0])

var sum = 0
for line in lines("input.txt"):
  if isRealRoom(line):
    sum += getSectorID(line)

echo sum