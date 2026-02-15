import os, strutils, algorithm

proc merge(r: seq[(int,int)]): seq[(int,int)] =
  if r.len == 0: return @[]
  var s = r
  s.sort(proc(a,b:(int,int)): int = a[0] - b[0])
  var m: seq[(int,int)] = @[]
  var cur = s[0]
  for i in 1..<s.len:
    let nxt = s[i]
    if nxt[0] <= cur[1] + 1:
      cur[1] = max(cur[1], nxt[1])
    else:
      m.add(cur)
      cur = nxt
  m.add(cur)
  m

proc contains(id:int, r: seq[(int,int)]): bool =
  var l = 0
  var h = r.len - 1
  while l <= h:
    let mid = (l + h) shr 1
    if id < r[mid][0]:
      h = mid - 1
    elif id > r[mid][1]:
      l = mid + 1
    else:
      return true
  false

let lines = readFile("input.txt").splitLines()
var ranges: seq[(int,int)] = @[]
var i = 0
while i < lines.len:
  let line = lines[i].strip()
  if line == "": break
  let p = line.split('-')
  ranges.add((parseInt(p[0]), parseInt(p[1])))
  inc i
inc i
var ids: seq[int] = @[]
while i < lines.len:
  for w in lines[i].splitWhitespace():
    ids.add(parseInt(w))
  inc i

let merged = merge(ranges)
var cnt = 0
for id in ids:
  if contains(id, merged): inc cnt
echo cnt