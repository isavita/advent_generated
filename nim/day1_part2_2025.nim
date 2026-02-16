
import std/[strutils, os]

proc floorDiv(a, b: int): int =
  result = a div b
  if a < 0 and a mod b != 0:
    dec result

proc slurp(path: string): string =
  var f = open(path)
  defer: close(f)
  result = f.readAll()

var pos = 50
var hits = 0
for line in slurp("input.txt").splitLines():
  if line.len == 0: continue
  let dir = line[0]
  let amt = line[1..^1].parseInt()
  if dir == 'R':
    let new = pos + amt
    hits += new div 100
    pos = new mod 100
  else:
    hits += floorDiv(pos - 1, 100) - floorDiv(pos - amt - 1, 100)
    pos = (pos - amt) mod 100
    if pos < 0: pos += 100
echo hits
