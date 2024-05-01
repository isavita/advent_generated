import std/[sequtils, strutils, streams, algorithm]

proc binaryToInt(binaryStr: string): int =
  result = 0
  for i, char in binaryStr:
    if char == '1':
      result = result or (1 shl (binaryStr.len - i - 1))

proc decode(pass: string): int =
  let row = binaryToInt(pass[0..6])
  let column = binaryToInt(pass[7..^1])
  return row * 8 + column

let file = newFileStream("input.txt", fmRead)
if file == nil:
  echo "Error opening file"
  quit(-1)

var seatIDs: seq[int]
for line in file.lines:
  let pass = line.replace("F", "0").replace("B", "1").replace("L", "0").replace("R", "1")
  seatIDs.add decode(pass)

seatIDs.sort(system.cmp[int]) # Use system.cmp[int] to specify the sorting order

for i in 0..<seatIDs.len - 1:
  if seatIDs[i + 1] != seatIDs[i] + 1:
    echo seatIDs[i] + 1
    break