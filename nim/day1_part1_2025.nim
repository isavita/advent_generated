
import os, strutils

proc main() =
  let data = readFile("input.txt").splitLines()
  const TOTAL = 100
  var pos = 50
  var count = 0
  for line in data:
    let line = line.strip()
    if line.len == 0: continue
    let dir = line[0]
    let dist = line[1..^1].parseInt()
    if dir == 'L': 
      pos = (pos - dist) mod TOTAL
    elif dir == 'R': 
      pos = (pos + dist) mod TOTAL
    if pos < 0: pos += TOTAL
    if pos == 0: inc count
  echo count

when isMainModule:
  main()
