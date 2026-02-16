
import std/[strutils, sequtils, os]

proc main() =
  let lines = readFile("input.txt").splitLines.filterIt(it.len > 0)
  if lines.len == 0:
    echo "Total rolls removed: 0"
    return
  let rows = lines.len
  let cols = lines[0].len
  var grid = newSeq[seq[char]](rows)
  for i in 0..<rows:
    grid[i] = cast[seq[char]](lines[i])
  var total = 0
  while true:
    var toRemove: seq[tuple[r, c: int]]
    for r in 0..<rows:
      for c in 0..<cols:
        if grid[r][c] == '@':
          var cnt = 0
          for dr in -1..1:
            for dc in -1..1:
              if dr != 0 or dc != 0:
                let nr = r + dr
                let nc = c + dc
                if nr >= 0 and nr < rows and nc >= 0 and nc < cols and grid[nr][nc] == '@':
                  inc cnt
          if cnt < 4:
            toRemove.add (r, c)
    if toRemove.len == 0:
      echo "Total rolls removed: ", total
      return
    inc total, toRemove.len
    for pos in toRemove:
      grid[pos.r][pos.c] = '.'

main()
