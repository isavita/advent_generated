
import strutils

var file = open("input.txt")
var totalDiff = 0

proc calculateEncodedLength(s: string): int =
  var encoded = "\""
  for ch in s:
    if ch == '\\' or ch == '"':
      encoded.add("\\")
    encoded.add(ch)
  encoded.add("\"")
  result = encoded.len

while not file.endOfFile:
  var line = file.readLine()
  var originalLength = line.len
  var encodedLength = calculateEncodedLength(line)
  totalDiff += encodedLength - originalLength

echo totalDiff
