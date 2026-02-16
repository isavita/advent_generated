import os

var lines: seq[string] = @[]
let f = open("input.txt")
for line in f.lines:
  if line.len > 0:
    lines.add(line)
f.close()

let rows = lines.len
let cols = if rows > 0: lines[0].len else: 0

let dx = [-1, -1, -1, 0, 0, 1, 1, 1]
let dy = [-1, 0, 1, -1, 1, -1, 0, 1]

var acc = 0
for y in 0..<rows:
  for x in 0..<cols:
    if lines[y][x] != '@': continue
    var cnt = 0
    for d in 0..<8:
      let nx = x + dx[d]
      let ny = y + dy[d]
      if nx >= 0 and nx < cols and ny >= 0 and ny < rows:
        if lines[ny][nx] == '@':
          cnt.inc
    if cnt < 4:
      acc.inc

echo acc