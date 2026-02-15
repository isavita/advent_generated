import std/[os, strutils, sets]

let txt = readFile("input.txt")
if txt.len == 0:
  echo 0
  quit()

let lines = txt.strip(trailing = true).splitLines()
var startRow = -1
var startCol = -1
for i, line in lines:
  let c = line.find('S')
  if c != -1:
    startRow = i
    startCol = c
    break

if startRow == -1:
  echo 0
  quit()

var beams = initHashSet[int]()
beams.incl(startCol)
var splits = 0

for r in startRow + 1 ..< lines.len:
  let row = lines[r]
  var nxt = initHashSet[int]()
  for c in beams:
    if c >= 0 and c < row.len:
      if row[c] == '^':
        inc splits
        nxt.incl(c - 1)
        nxt.incl(c + 1)
      else:
        nxt.incl(c)
  beams = nxt
  if beams.len == 0:
    break

echo splits