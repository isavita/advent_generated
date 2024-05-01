import std/[streams, strutils]

proc calculateMemoryLength(s: string): int =
  var length = 0
  var inEscape = false
  var hexCount = 0

  for i in 1 ..< s.len - 1:
    if hexCount > 0:
      dec hexCount
    elif inEscape:
      if s[i] == 'x':
        hexCount = 2
      inEscape = false
      inc length
    elif s[i] == '\\':
      inEscape = true
    else:
      inc length

  return length

var file = newFileStream("input.txt", fmRead)
if file == nil:
  echo "Error opening file"
  quit 1

var totalDiff = 0
for line in file.lines:
  let codeLength = len(line)
  let memoryLength = calculateMemoryLength(line)
  totalDiff += codeLength - memoryLength

echo totalDiff