
import tables, strutils

var grid: seq[string]
try:
  for line in lines("input.txt"):
    var l = line
    if l.endsWith("\r"): l.setLen(l.len - 1)
    if l.len > 0: grid.add l
except IOError: discard

if grid.len == 0:
  echo 0
  quit()

var (sx, sy) = (-1, -1)
for y, row in grid:
  let x = row.find('S')
  if x != -1:
    (sx, sy) = (x, y)
    break

if sx == -1: quit()

var counts = {sx: 1'u64}.toTable
let w = grid[0].len

for y in sy ..< grid.len:
  var next = initTable[int, uint64]()
  for x, cnt in counts:
    if x >= 0 and x < w and grid[y][x] == '^':
      next.mgetOrPut(x - 1, 0'u64) += cnt
      next.mgetOrPut(x + 1, 0'u64) += cnt
    else:
      next.mgetOrPut(x, 0'u64) += cnt
  counts = next

var res = 0'u64
for v in counts.values:
  res += v
echo res

