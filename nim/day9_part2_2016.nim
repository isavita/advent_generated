import strutils, sequtils, parseutils, strformat

proc decompress(s: string): int =
  var i = 0
  var total = 0
  while i < s.len:
    if s[i] == '(':
      var j = i + 1
      var marker = ""
      while s[j] != ')':
        marker.add(s[j])
        j += 1
      let parts = marker.split('x')
      let repeatCount = parseInt(parts[1])
      let repeatLength = parseInt(parts[0])
      i = j + 1
      total += repeatCount * decompress(s[i..i + repeatLength - 1])
      i += repeatLength
    else:
      total += 1
      i += 1
  return total

let input = readFile("input.txt").strip()
echo decompress(input)