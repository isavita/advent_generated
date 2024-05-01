import strutils, sequtils, parseutils

proc decompressLength(s: string): int =
  var i = 0
  var length = 0
  while i < s.len:
    if s[i] == '(':
      var j = i + 1
      var marker = ""
      while s[j] != ')':
        marker.add(s[j])
        j.inc
      let parts = marker.split("x")
      let count = parseInt(parts[0])
      let repeatCount = parseInt(parts[1])
      i = j + 1
      length += count * repeatCount
      i += count
    else:
      length.inc
      i.inc
  return length

let input = readFile("input.txt").strip()
echo decompressLength(input)